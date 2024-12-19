(ns day17.solution
  (:require 
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day17/input"
    slurp
    string/trim 
    string/split-lines))


(defn- parse-input [input]
  (let [[a b c _ program] input
        [_ a] 			  (re-matches #"Register A: (\d+)" a)
        [_ b] 			  (re-matches #"Register B: (\d+)" b)
        [_ c] 			  (re-matches #"Register C: (\d+)" c)
        [_ program] 	  (re-matches #"Program: (\d(,\d)*)" program)
        a 				  (parse-long a)
        b 				  (parse-long b)
        c 				  (parse-long c)
        program 		  (mapv parse-long (string/split program #","))]
    [a b c program]))


(defn- read-combo [program pc a b c]
  (case (get program (inc pc))
    0 0
    1 1
    2 2
    3 3
    4 a
    5 b
    6 c))


(defn- run [a b c program]
  (loop [a 		a
         b 		b
         c 		c
         pc 	0
         output []]
    (case (get program pc)
      nil output
      0   (recur (unsigned-bit-shift-right a (read-combo program pc a b c)) b c (+ pc 2) output)
      1   (recur a (bit-xor b (get program (inc pc))) c (+ pc 2) output)
      2   (recur a (bit-and 7 (read-combo program pc a b c)) c (+ pc 2) output)
      3   (recur a b c (if (zero? a) (+ pc 2) (get program (inc pc))) output)
      4   (recur a (bit-xor b c) c (+ pc 2) output)
      5   (recur a b c (+ pc 2) (conj output (bit-and 7 (read-combo program pc a b c))))
      6   (recur a (unsigned-bit-shift-right a (read-combo program pc a b c)) c (+ pc 2) output)
      7   (recur a b (unsigned-bit-shift-right a (read-combo program pc a b c)) (+ pc 2) output))))


(defn- combo-to-str [operand]
  (case operand
    0 "0"
    1 "1"
    2 "2"
    3 "3"
    4 "a"
    5 "b"
    6 "c"))


(defn- extract-aoc-special-program [program]
  (loop [pc 0 state 0 had-shr false special-program []]
    (let [[opcode operand] (drop pc program)]
      (cond
        (= [state opcode operand] [0 2 4]) 		(recur (+ pc 2) 1 false special-program)
        (= [state opcode] [1 1]) 		   		(recur (+ pc 2) 2 false (conj special-program operand))
        (= [state opcode operand] [2 7 5]) 		(recur (+ pc 2) 3 false special-program)
        (= [state opcode] [3 4]) 		   		(recur (+ pc 2) 4 had-shr special-program)
        (= [state opcode] [4 1]) 		   		(recur (+ pc 2) 5 had-shr (conj special-program operand))
        (= [state opcode operand] [5 5 5]) 		(recur (+ pc 2) 6 had-shr special-program)
        (= [state opcode operand] [6 3 0]) 		(recur (+ pc 2) 7 had-shr special-program)
        (and (= state 7) had-shr (nil? opcode)) special-program
        (and (>= state 3) (not had-shr))   		(recur (+ pc 2) state true special-program)
        :else 									nil))))


(defn- find-a-special-program [program special-program a]
  (if (empty? program)
    a
    (->> (range 8)
      (filter (fn [b]
                (let [b1 (bit-xor b  (first special-program))
                      b1 (bit-xor b1 (bit-shift-right (bit-or (bit-shift-left a 3) b) b1))
                      b1 (bit-xor b1 (last special-program))]
                  (= (bit-and b1 7) (last program)))))
      (map (fn [b] (find-a-special-program (pop program) special-program (bit-or (bit-shift-left a 3) b))))
      (filter some?)
      (first))))


(defn- find-a-regular-program [program b c]
  (first
    (filter
      (fn [a] (= program (run a b c program)))
      (range))))


(def ^:private part1 
  (let [[a b c program] (parse-input input)]
    (string/join ",", (run a b c program))))


(def ^:private part2 
  (let [[_ b c program] (parse-input input)
        special-program (extract-aoc-special-program program)]
    (if (nil? special-program)
      (find-a-regular-program program b c)
      (find-a-special-program program special-program 0))))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))
