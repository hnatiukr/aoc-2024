(ns day6.solution
  (:require 
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day6/input"
    slurp
    string/trim 
    string/split-lines))


(defn- parse-graph [input]
  (let [cells (mapv #(string/split % #"") input)
        cells (map-indexed (fn [row-idx row] (map-indexed (fn [col-idx cell] [[row-idx col-idx] cell]) row)) cells)
        cells (apply concat cells)
        graph (into {} cells)]
    graph))


(defn- turn-right [dir]
  (condp = dir 
    "^" ">"
    ">" "v"
    "v" "<"
    "<" "^"))


(defn- move [[x y] dir]
  (condp = dir 
    "^" [(- x 1) y]
    ">" [x (+ y 1)]
    "v" [(+ x 1) y]
    "<" [x (- y 1)]))


(defn- find-guard [graph]
  (let [[[x y] dir] (first (filterv #(= (second %) "^") graph))]
    {:coord [x y] 
     :dir 	dir}))


(defn- next-step [guard graph]
  (let [next-coord (move (:coord guard) (:dir guard))
        cell 	   (get graph next-coord)]
    (cond
      (= cell "#") (assoc guard :dir (turn-right (:dir guard)))
      (= cell ".") (assoc guard :coord next-coord)
      (= cell "^") (assoc guard :coord next-coord)
      :else 	   nil)))


(defn- find-obstructions [graph]
  (sort-by second (sort-by first (filterv #(= "#" (get graph %)) (keys graph)))))


(defn- next-far-step [guard graph obstructions]
  (let [dir 	   (:dir guard)
        [gx gy]    (:coord guard)
        next-coord (condp = dir
                     "^" (let [next (filterv (fn [[x y]] (and (= gy y) (< x gx))) obstructions)
                               next (if (empty? next) nil (inc (first (last next))))]
                           [next gy])
                     ">" (let [next (filterv (fn [[x y]] (and (= gx x) (> y gy))) obstructions)
                               next (if (empty? next) nil (dec (second (first next))))]
                           [gx next])
                     "v" (let [next (filterv (fn [[x y]] (and (= gy y) (> x gx))) obstructions)
                               next (if (empty? next) nil (dec (first (first next))))]
                           [next gy])
                     "<" (let [next (filterv (fn [[x y]] (and (= gx x) (< y gy))) obstructions)
                               next (if (empty? next) nil (inc (second (last next))))]
                           [gx next]))]
    (if (contains? graph next-coord)
      {:coord next-coord 
       :dir   (turn-right dir)}
      nil)))


(defn- find-path [graph]
  (let [guard (find-guard graph)]
    (loop [prev-guard guard
           path 	  [guard]]
      (let [next-guard (next-step prev-guard graph)]
        (if (nil? next-guard)
          (mapv :coord path)
          (recur next-guard (conj path next-guard)))))))


(defn- cycle? [path]
  (let [coords-freq (frequencies path)]
    (> (apply max (vals coords-freq)) 2)))


(defn- leads-to-cycle? [graph]
  (let [guard 		 (find-guard graph)
        obstructions (find-obstructions graph)]
    (loop [prev-guard guard
           path 	  [guard]]
      (let [next-guard (next-far-step prev-guard graph obstructions)]
        (cond
          (nil? next-guard) 		  false
          (cycle? (mapv :coord path)) true
          :else 					  (recur next-guard (conj path next-guard)))))))


(def part1 
  (let [graph (parse-graph input)
        path  (find-path graph)]
    (count (set path))))


(def part2 
  (let [graph (parse-graph input)
        guard (find-guard graph)
        path  (vec (set (find-path graph)))
        path  (filterv #(not= (:coord guard) %) path)]
    (loop [positions   path
           cycle-count 0]
      (if (empty? positions)
        cycle-count
        (let [position  (first positions)
              new-graph (assoc graph position "#")]
          (if (leads-to-cycle? new-graph)
            (recur (rest positions) (inc cycle-count))
            (recur (rest positions) cycle-count)))))))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))