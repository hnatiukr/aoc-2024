(ns day14.solution
  (:require 
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day14/input"
    slurp
    string/trim 
    string/split-lines))


(def ^:private WIDTH 101)
(def ^:private HEIGHT 103)


(defn- parse-input [input]
  (map
    (fn [line]
      (vec (map parse-long (drop 1 (re-matches #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)" line)))))
    input))


(defn- quadrant-counts [iterations input]
  (->> input
    (map
      (fn [[x y vx vy]]
        (let [x (mod (+ x (* iterations vx)) WIDTH) 
              y (mod (+ y (* iterations vy)) HEIGHT)]
          (cond
            (< x (quot WIDTH 2)) (cond
                                   (< y (quot HEIGHT 2)) [1 0 0 0]
                                   (> y (quot HEIGHT 2)) [0 1 0 0]
                                   :else 				 [0 0 0 0]
                                   )
            (> x (quot WIDTH 2)) (cond
                                   (< y (quot HEIGHT 2)) [0 0 1 0]
                                   (> y (quot HEIGHT 2)) [0 0 0 1]
                                   :else 				 [0 0 0 0])
            :else [0 0 0 0]))))
    (reduce
      (partial mapv +)
      [0 0 0 0])))


(defn- run [input iterations]
  (->> input
    (quadrant-counts iterations)
    (reduce *)))


(defn- is-christmas-tree [iterations input]
  (let [positions (into #{} 
                    (map 
                      (fn [[x y vx vy]] [(mod (+ x (* iterations vx)) WIDTH) (mod (+ y (* iterations vy)) HEIGHT)]) 
                      input))]
    (some (fn [[x y]] (every? #(contains? positions [(+ x %) y]) (range 15))) positions)))


(def ^:private part1 
  (run (parse-input input) 100))


(def ^:private part2 
  (let [input (parse-input input)]
    (first (filter #(is-christmas-tree % input) (iterate inc 0)))))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))