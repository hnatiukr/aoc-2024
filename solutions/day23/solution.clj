(ns day23.solution
  (:require 
   [clojure.string :as string]
   [clojure.math.combinatorics :as combinatorics]))


(def ^:private input
  (-> "solutions/day23/input"
    slurp
    string/trim 
    string/split-lines))


(defn- parse-input [input]
  (->> input
    (map (fn [line]
           (let [[a b] (string/split line #"-" 2)]
             [{a #{b}} {b #{a}}])))
    (apply concat)
    (apply merge-with into)))


(defn- is-complete [graph subgraph]
  (every? 
    (fn [[a b]] (contains? (get graph a) b)) 
    (combinatorics/combinations subgraph 2)))


(def part1
  (let [graph 	(parse-input input)
        triples (set
                  (apply concat
                    (map (fn [[a values]]
                           (map
                             (fn [[b c]] #{a b c})
                             (filter
                               (fn [[b c]] (contains? (get graph b) c))
                               (combinatorics/combinations values 2))))
                      graph)))]
    (count (filter 
             (fn [triple] (some #(string/starts-with? % "t") triple)) 
             triples))))


(def part2
  (let [graph 			  (parse-input input)
        max-subgraph-size (inc (apply max (map count (vals graph))))]
    (->> (range max-subgraph-size 0 -1)
      (map
        (fn [subgraph-size]
          (filter
            #(seq %)
            (map
              (fn [[key val]]
                (filter
                  (fn [subgraph]
                    (and
                      (= key (reduce (fn [a b] (if (neg? (compare a b)) a b)) subgraph))
                      (is-complete graph subgraph)))
                  (combinatorics/combinations (vec (conj val key)) subgraph-size)))
              graph))))
      (filter #(seq %))
      (first)
      (first)
      (first)
      (sort)
      (string/join ","))))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))