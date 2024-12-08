(ns day7.solution
  (:require 
   [clojure.math :as math]
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day7/input"
    slurp
    string/trim 
    string/split-lines))


(defn- parse-input [input]
  (map (fn [line] 
         (let [[result expr] (string/split line #": " 2)]
           [(parse-long result) (map parse-long (string/split expr #" "))]))
    input))


(defn- powi [base exp]
  (loop [base base exp exp result 1]
    (cond
      (= exp 0)   result
      (= exp 1)   (* result base)
      (even? exp) (recur (* base base) (bit-shift-right exp 1) result)
      :else 	  (recur (* base base) (bit-shift-right exp 1) (* result base)))))


(defn- run [input num-operators]
  (->> 
    (parse-input input)
    (filter (fn [[result expr]]
              (some
                (fn [combo] (->> 
                              expr
                              (map (fn [x] [0 x]))
                              (reduce
                                (fn [[i x] [_ y]] [(inc i)
                                                   (case (mod (quot combo (powi num-operators i)) num-operators)
                                                     0 (+ x y)
                                                     1 (* x y)
                                                     2 (+ y (* x (long (math/pow 10 (inc (math/floor (math/log10 y))))))))]))
                              (last)
                              (= result)))
                (range (powi num-operators (dec (count expr)))))))
    (map first)
    (reduce +)))


(def part1 
  (run input 2))


(def part2 
  (run input 3))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))