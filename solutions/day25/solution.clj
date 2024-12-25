(ns day25.solution
  (:require 
   [clojure.string :as string]
   [clojure.math.combinatorics :as combinatorics]))


(def ^:private input
  (-> "solutions/day25/input"
    slurp
    string/trim 
    string/split-lines))


(defn- grid-coords [grid]
  (apply concat (map-indexed 
                  (fn [y line] (map (fn [x] [x y]) (range (count line)))) 
                  grid)))


(defn- count-matches [func seq]
  (reduce + (map #(if (func %) 1 0) seq)))


(defn- parse-input [input]
  (let [lines (vec input)]
    (assert (= 7 (rem (count lines) 8)))
    (mapv vec (partition 8 (conj lines "")))))


(def ^:private part1 
  (count-matches
    (fn [[first second]]
      (not-any?
        (fn [[x y]] (= \# (get-in first [y x]) (get-in second [y x])))
        (grid-coords first)))
    (combinatorics/combinations (parse-input input) 2)))


(def ^:private part2 
  nil)


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))