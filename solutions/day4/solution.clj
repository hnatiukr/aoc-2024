(ns day4.solution
  (:require 
   [clojure.string :as string]))


(def input
  (-> "solutions/day4/input"
    slurp
    string/trim 
    string/split-lines))


(def ^:private directions [[-1 -1]
                           [0  -1]
                           [1  -1]
                           [-1 0]
                           [1  0]
                           [-1 1]
                           [0  1]
                           [1  1]])

(defn- run [input count-matches-at-func]
  (let [wordsearch input]
    (reduce +
      (map (fn [y] (reduce 
                     + 
                     (map 
                       (fn [x] (count-matches-at-func wordsearch x y))
                       (range (count (get wordsearch y)))))) 
        (range (count wordsearch))))))


(def part1 (run input
             (fn [wordsearch x y]
               (reduce + 
                 (map #(if ((fn [[dx dy]]
                              (and
                                (= \X (get-in wordsearch [y x]))
                                (= \M (get-in wordsearch [(+ y dy) (+ x dx)]))
                                (= \A (get-in wordsearch [(+ y (* dy 2)) (+ x (* dx 2))]))
                                (= \S (get-in wordsearch [(+ y (* dy 3)) (+ x (* dx 3))]))
                                )) %) 
                         1 
                         0) 
                   directions)))))


(def part2 
  (run input
    (fn [wordsearch x y]
      (if (and
            (= \A (get-in wordsearch [y x]))
            (or
              (and
                (= \M (get-in wordsearch [(- y 1) (- x 1)]))
                (= \S (get-in wordsearch [(+ y 1) (+ x 1)]))
                )
              (and
                (= \S (get-in wordsearch [(- y 1) (- x 1)]))
                (= \M (get-in wordsearch [(+ y 1) (+ x 1)]))))
            (or
              (and
                (= \M (get-in wordsearch [(- y 1) (+ x 1)]))
                (= \S (get-in wordsearch [(+ y 1) (- x 1)]))
                )
              (and
                (= \S (get-in wordsearch [(- y 1) (+ x 1)]))
                (= \M (get-in wordsearch [(+ y 1) (- x 1)])))))
        1 
        0))))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))
