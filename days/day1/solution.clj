(ns day1.solution
  (:require 
   [clojure.string :as string]))


(def input
  (-> "days/day1/input"
    slurp
    string/trim 
    string/split-lines))


(def split-ids (reduce
                 (fn [acc pair] 
                   (let [[left right] (string/split pair #"   ")
                         left-id      (Integer/parseInt (string/trim left))
                         right-id     (Integer/parseInt (string/trim right))] 
                     (assoc acc 
                       :left  (conj (:left acc) left-id) 
                       :right (conj (:right acc) right-id))))
                 {:left  [] 
                  :right []}
                 input))


(def sorted-left-ids (sort-by min (:left split-ids)))


(def sorted-right-ids (sort-by min (:right split-ids)))


(def part1 
  (reduce 
    (fn [sum [left-id right-id]]
      (+ sum (abs (- left-id right-id)))) 
    0 
    (map vector sorted-left-ids sorted-right-ids)))


(def part2 
  (reduce 
    (fn [similarity-score left-id] 
      (+ similarity-score (* left-id (count (filter #(= % left-id) 
                                              (vec sorted-right-ids))))))
    0
    sorted-left-ids))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))