(ns day19.solution
  (:require 
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day19/input"
    slurp
    string/trim 
    string/split-lines))


(defn- parse-input [input]
  (let [lines 	 input
        patterns (string/split (first lines) #", ")
        designs  (drop 2 lines)]
    [patterns designs]))


(defn- is-design-possible [design patterns]
  (or
    (empty? design)
    (some
      (fn [pattern] 
        (and
          (string/starts-with? design pattern)
          (is-design-possible (subs design (count pattern)) patterns)))
      patterns)))


(defn count-matches [func seq]
  (reduce + (map #(if (func %) 1 0) seq)))


(defn- count-combinations [designs patterns]
  (let [count-combinations' (promise)]
    (deliver count-combinations' 
      (memoize (fn [design]
                 (if (empty? design)
                   1
                   (reduce +
                     (map
                       #(@count-combinations' (subs design (count %)))
                       (filter #(string/starts-with? design %) patterns)))))))
    (reduce + (map @count-combinations' designs))))


(def ^:private part1 
  (let [[patterns designs] (parse-input input)]
    (count-matches (fn [design] (is-design-possible design patterns)) designs)))


(def ^:private part2 
  (let [[patterns designs] (parse-input input)]
    (count-combinations designs patterns)))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))