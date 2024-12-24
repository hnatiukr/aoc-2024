(ns day24.solution
  (:require 
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day24/input"
    slurp
    string/trim 
    string/split-lines))


(defn- parse-input [input]
  (let [lines 				   input
        [initial-states gates] (split-with #(seq %) lines)
        gates 				   (drop 1 gates)
        initial-states 		   (into {} (map
                                       (fn [line]
                                         (let [[key val] (string/split line #": " 2)]
                                           [key (= val "1")]))
                                       initial-states))
        gates 				  (mapv #(vec (drop 1 (re-matches #"(\w+) (AND|OR|XOR) (\w+) -> (\w+)" %))) gates)]
    [initial-states gates]))


(defn- read-output [letter state]
  (->> state
    (filter (fn [[k]] (string/starts-with? k letter)))
    (map (fn [[k v]] (if v (bit-shift-left 1 (parse-long (subs k 1))) 0)))
    (reduce bit-or)))


(defn- simulate [state gates]
  (loop [state state]
    (let [[changed state] (reduce
                            (fn [[changed state] [in1 op in2 out]]
                              (if (or (contains? state out) (not (contains? state in1)) (not (contains? state in2)))
                                [changed state]
                                (let [val (case op
                                            "AND" (and (get state in1) (get state in2))
                                            "OR"  (or (get state in1) (get state in2))
                                            "XOR" (not= (get state in1) (get state in2)))]
                                  [true (assoc state out val)])))
                            [false state]
                            gates)]
      (if changed
        (recur state)
        (read-output "z" state)))))


(def ^:private part1 
  (let [[state gates] (parse-input input)]
    (simulate state gates)))


(def ^:private part2 
  (let [[state gates] (parse-input input)
        x			  (read-output "x" state)
        y			  (read-output "y" state)
        z			  (simulate state gates)
        expected_z 	  (+ x y)]
    (println "// Expected :" (Long/toBinaryString expected_z))
    (println "// Actual   :" (Long/toBinaryString z))))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))