(ns day20.solution
  (:require 
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day20/input"
    slurp
    string/trim 
    string/split-lines))


(def ^:private directions [[1 0] [0 1] [-1 0] [0 -1]])


(defn- count-matches [func seq]
  (reduce + (map #(if (func %) 1 0) seq)))



(defn- grid-coords [grid]
  (apply concat (map-indexed (fn [y line] (map (fn [x] [x y]) (range (count line)))) grid)))


(defn- find-in-grid [grid predicate]
  (first (filter (fn [[x y]] (predicate (get-in grid [y x]))) (grid-coords grid))))


(defn- bfs [grid start-x start-y end-x end-y]
  (loop [queue 	   [[start-x start-y]]
         distances {[start-x start-y] 0}]
    (let [[[x y]] queue]
      (if (or (nil? x) (= [x y] [end-x end-y]))
        distances
        (let [[queue distances] 
              (reduce
                (fn [[queue distances] [dx dy]]
                  (let [x1 (+ x dx) y1 (+ y dy)]
                    (if
                      (or
                        (= \# (get-in grid [y1 x1]))
                        (and (contains? distances [x1 y1]) (>= (inc (get distances [x y])) (get distances [x1 y1]))))
                      [queue distances]
                      [(conj queue [x1 y1]) (assoc distances [x1 y1] (inc (get distances [x y])))])))
                [(vec (rest queue)) distances]
                directions)]
          (recur queue distances))))))


(defn- run [input max-cheat-time]
  (let [grid input
        [start-x start-y] (find-in-grid grid #(= % \S))
        [end-x end-y] 	  (find-in-grid grid #(= % \E))
        distances 		  (bfs grid start-x start-y end-x end-y)]
    (reduce +
      (map
        (fn [[x y]]
          (let [dist1 (get distances [x y])]
            (if (nil? dist1)
              0
              (reduce +
                (map
                  (fn [dy]
                    (count-matches
                      (fn [dx]
                        (and
                          (<= (+ (abs dx) (abs dy)) max-cheat-time)
                          (let [dist2 (get distances [(+ x dx) (+ y dy)])]
                            (and (some? dist1) (some? dist2) (>= dist2 (+ dist1 100 (abs dx) (abs dy)))))))
                      (range (- max-cheat-time) (inc max-cheat-time))))
                  (range (- max-cheat-time) (inc max-cheat-time)))))))
        (grid-coords grid)))))


(def ^:private part1 
  (run input 2))


(def ^:private part2 
  (run input 20))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))