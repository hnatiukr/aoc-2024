(ns day22.solution
  (:require 
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day22/input"
    slurp
    string/trim 
    string/split-lines))


(defn- simulate-xorshift [value]
  (let [value (bit-and (bit-xor value (bit-shift-left value 6)) 0xffffff)
        value (bit-and (bit-xor value (bit-shift-right value 5)) 0xffffff)
        value (bit-and (bit-xor value (bit-shift-left value 11)) 0xffffff)]
    value))


(defn- all-patterns [seeds]
  (set
    (apply concat
      (map
        (fn [seed]
          (map
            (fn [window] (mapv (fn [[a b]] (- b a)) (partition 2 1 window)))
            (partition 5 1 (take 2001 (map #(rem % 10) (iterate simulate-xorshift seed))))))
        seeds))))


(defn- make-pattern [seed]
  (reduce
    (fn [m [k v]] (if (contains? m k) m (assoc m k v)))
    {}
    (map
      (fn [window]
        [(mapv (fn [[a b]] (- b a)) (partition 2 1 window)) (last window)])
      (partition 5 1 (map #(rem % 10) (take 2001 (iterate simulate-xorshift seed)))))))


(def ^:private part1 
  (let [seeds (map parse-long input)]
    (reduce + (map #(nth (iterate simulate-xorshift %) 2000) seeds))))


(def ^:private part2 
  (let [seeds 		   (map parse-long input)
        pattern-prices (map make-pattern seeds)]
    (reduce max
      (map
        (fn [pattern]
          (reduce +
            (map
              (fn [pattern-prices] (get pattern-prices pattern 0))
              pattern-prices)))
        (all-patterns seeds)))))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))