(ns day5.solution
  (:require
   [clojure.string :as string]))


(def input
  (-> "solutions/day5/input"
    slurp
    string/trim
    string/split-lines))


(defn- parse-input [input]
  (let [lines              input
        [rules page-lists] (split-with #(seq %) lines)
        page-lists         (drop 1 page-lists)]
    [(map (fn [rule] (mapv parse-long (string/split rule #"\|"))) rules)
     (map (fn [page-list] (mapv parse-long (string/split page-list #","))) page-lists)]))


(defn- wrong-order? [reverse-graph a b]
  (contains? (get reverse-graph a #{}) b))


(defn- valid? [reverse-graph page-list]
  (let [windows (partition 2 1 page-list)]
    (not-any? (fn [[a b]] (wrong-order? reverse-graph a b)) windows)))


(defn- proceed [input process-page-list-func]
  (let [[rules page-lists] (parse-input input)
        reverse-graph      (reduce (fn [map [a b]] (merge-with into map {b #{a}})) {} rules)]
    (reduce + (map #(process-page-list-func reverse-graph %) page-lists))))


(def part1
  (proceed input
    (fn [reverse-graph page-list]
      (if (valid? reverse-graph page-list)
        (get page-list (quot (count page-list) 2))
        0))))


(def part2
  (proceed input
    (fn [reverse-graph page-list]
      (if (valid? reverse-graph page-list)
        0
        (let [page-list (sort
                          (fn [a b] (cond
                                      (wrong-order? reverse-graph a b) 1
                                      (wrong-order? reverse-graph b a) -1
                                      :else 0))
                          page-list)
              page-list (vec page-list)]
          (get page-list (quot (count page-list) 2)))))))

(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))
