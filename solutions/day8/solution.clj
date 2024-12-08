(ns day8.solution
  (:require 
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day8/input"
    slurp
    string/trim 
    string/split-lines))


(defn- parse-graph [input]
  (let [lines input
        lines (mapv #(string/split % #"") lines)
        cells (map-indexed (fn [row line] (map-indexed (fn [col cell] [[row col] cell]) line)) lines)
        cells (apply concat cells)]
    (into {} cells)))


(defn- diff [[ax ay] [bx by]]
  [(- ax bx) (- ay by)])


(defn- by-cells [graph]
  (into {} (for 
             [[k v] (group-by first (mapv (fn [val] (vec (reverse val))) (into [] graph)))] 
             [k (vec (apply concat (mapv rest v)))])))


(defn- repeat-antinode [graph x y dx dy]
  (loop [stop 	   false
         current   [x y]
         antinodes (set [])]
    (if stop
      antinodes
      (let [[x y] current
            next  [(+ x dx) (+ y dy)]]
        (if (contains? graph next)
          (recur false next (conj antinodes next))
          (recur true next antinodes))))))


(defn- find-antinodes [repeatable graph graph-by-cells [x y]]
  (let [freq 		(get graph [x y])
        other-cells (filterv (fn [[ox oy]] (not= [x y] [ox oy])) (graph-by-cells freq))
        antinodes   (mapv (fn [[ox oy]] 
                            (let [[dx dy] (diff [x y] [ox oy])]
                              (if repeatable
                                (repeat-antinode graph x y dx dy)
                                [[(+ x dx) (+ y dy)]]))) 
                      other-cells)]
    (apply concat antinodes)))


(defn- find-all-antinodes [repeatable graph graph-by-cells]
  (loop [cells 	   (filterv (fn [[[_x _y] freq]] (not= "." freq)) (into [] graph))
         antinodes (set [])]
    (if (empty? cells)
      antinodes
      (let [[[x y]] (first cells)
            freq-antinodes (find-antinodes repeatable graph graph-by-cells [x y])
            freq-antinodes (filter #(contains? graph %) freq-antinodes)]
        (recur (rest cells) (into antinodes freq-antinodes))))))


(def ^:private part1 
  (let [graph 		   (parse-graph input)
        graph-by-cells (by-cells graph)
        all-antinodes  (find-all-antinodes false graph graph-by-cells)
        all-antinodes  (set all-antinodes)] 
    (count all-antinodes)))


(def ^:private part2 
  (let [graph 		   (parse-graph input)
        graph-by-cells (by-cells graph)
        antennas 	   (mapv first (filterv (fn [[[_x _y] freq]] (not= "." freq)) (into [] graph)))
        all-antinodes  (find-all-antinodes true graph graph-by-cells)
        all-antinodes  (set all-antinodes)
        all-antinodes  (into all-antinodes antennas)]
    (count all-antinodes)))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))