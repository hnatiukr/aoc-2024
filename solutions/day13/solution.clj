(ns day13.solution
  (:require 
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day13/input"
    slurp
    string/trim 
    string/split-lines))


(defn- parse-input [input]
  (->> input
    (partition-by empty?)
    (remove #(empty? (first %)))
    (map (fn [[a-text b-text prize-text]]
           (let [[_ ax ay]           (re-matches #"Button A: X\+(\d+), Y\+(\d+)" a-text)
                 [_ bx by]           (re-matches #"Button B: X\+(\d+), Y\+(\d+)" b-text)
                 [_ prize-x prize-y] (re-matches #"Prize: X=(\d+), Y=(\d+)" prize-text)]
             (mapv parse-long [ax ay bx by prize-x prize-y]))))))


(defn- run [input]
  (reduce +
    (map
      (fn [[a c b d prize-x prize-y]]
        (let [det         (- (* a d) (* b c))
              _           (assert (not (zero? det)) "found non-invertible matrix in input")
              adj-a       d
              adj-b       (- b)
              adj-c       (- c)
              adj-d       a
              adj-prize-x (+ (* adj-a prize-x) (* adj-b prize-y))
              adj-prize-y (+ (* adj-c prize-x) (* adj-d prize-y))]
          (if (and (zero? (mod adj-prize-x det)) (zero? (mod adj-prize-y det)))
            (+ (* 3 (quot adj-prize-x det)) (quot adj-prize-y det))
            0)))
      input)))


(def ^:private part1 
  (run (parse-input input)))


(def ^:private part2 
  (run (map (fn [[ax ay bx by prize-x prize-y]] 
              [ax ay bx by (+ 10000000000000 prize-x) (+ 10000000000000 prize-y)]) 
         (parse-input input))))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))