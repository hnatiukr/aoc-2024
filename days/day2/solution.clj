(ns day2.solution
  (:require 
   [clojure.string :as string]))


(def input
  (->> "days/day2/input"
    slurp
    string/trim 
    string/split-lines
    (map #(string/split % #" "))
    (mapv #(mapv Integer/parseInt %))))


(defn- safe-report? [report]
  (let [adjacent-levels  (partition 2 1 report)
        differences 	 (for [[a b] adjacent-levels] (- b a))]
    (or
      (every? #{1  2  3} differences)
      (every? #{-1 -2 -3} differences))))


(defn- contains-single-bad-level? [report]
  (loop [idx (dec (count report))]
    (let [pre     (subvec report 0 idx)
          post    (subvec report (inc idx))
          report' (into pre post)]
      (cond
        (safe-report? report') true
        (zero? idx)   		   false
        :else         		   (recur (dec idx))))))


(defn- tolerated-unsafe-report? [report]
  ((some-fn safe-report? contains-single-bad-level?) report))


(def part1 (count 
             (filter safe-report? input)))


(def part2 (count 
             (filter tolerated-unsafe-report? input)))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))

