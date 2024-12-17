(ns day16.solution
  (:require 
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day16/input"
    slurp
    string/trim 
    string/split-lines))


(def ^:private directions [[1 0] [0 1] [-1 0] [0 -1]])


(defn grid-coords [grid]
  (apply concat 
    (map-indexed (fn [y line] 
                   (map 
                     (fn [x] [x y]) 
                     (range (count line)))) grid)))


(defn- find-in-grid [grid predicate]
  (first 
    (filter 
      (fn [[x y]] (predicate (get-in grid [y x]))) 
      (grid-coords grid))))


(defn- shortest-distances-from
  ([from adjacent-func] 
   (shortest-distances-from from #(false) adjacent-func))
  ([from stop-at adjacent-func]
   (loop [distances {from 0}
          to-visit  (sorted-set [0 from])]
     (let [[dist node] (first to-visit)]
       (cond
         (nil? dist) 	[nil distances]
         (stop-at node) [node distances]
         :else 			(let [adjacent (map 
                                   (fn [[n d]] [n (+ dist d)]) 
                                   (adjacent-func node))
                    	  	to-visit (into
                                   (disj to-visit [dist node])
                                   (map
                                     (fn [[n d]] [d n])
                                     (filter
                                       (fn [[n d]]
                                         (let [prev-d (get distances n)]
                                           (or (nil? prev-d) (< d prev-d))))
                                       adjacent)))
                        distances (merge-with min distances (into {} adjacent))]
                    (recur distances to-visit)))))))


(defn- shortest-distance [from is-dest? adjacent-func]
  (let [[dest distances] (shortest-distances-from from is-dest? adjacent-func)] 
    (get distances dest)))


(defn- neighbors
  ([grid node] (neighbors grid node false))
  ([grid node reverse]
   (let [[x y dir] node]
     (into
       (let [[x1 y1] (mapv + [x y] (get directions 
                                     (if reverse 
                                       (bit-xor dir 2) 
                                       dir)))]
         (if (= (get-in grid [y1 x1]) \#) 
           [] 
           [[[x1 y1 dir] 1]]))
       (map
         (fn [dir] [[x y dir] 1000])
         (remove #(= dir %) (range 4)))))))


(defn- nodes-on-shortest-paths [grid distances from nodes]
  (let [distance (get distances from)]
    (reduce
      (fn [nodes [node dist]]
        (cond
          (contains? nodes node) 					 nodes
          (= (- distance dist) (get distances node)) (nodes-on-shortest-paths grid distances node nodes)
          :else 				 					 nodes))
      (conj nodes from)
      (neighbors grid from true))))


(def ^:private part1 
  (let [grid 			  input
        [start-x start-y] (find-in-grid grid #(= \S %))
        end-pos 		  (find-in-grid grid #(= \E %))]
    (shortest-distance
      [start-x start-y 0]
      (fn [[x y]] (= [x y] end-pos))
      (fn [node] (neighbors grid node)))))


(def ^:private part2 
  (let [grid 			     input
        [start-x start-y] 	 (find-in-grid grid #(= \S %))
        end-pos 			 (find-in-grid grid #(= \E %))
        [end-node distances] (shortest-distances-from
                               [start-x start-y 0]
                               (fn [[x y]] (= [x y] end-pos))
                               (fn [node] (neighbors grid node)))]
    (count (distinct (map 
                       (fn [[x y]] [x y]) 
                       (nodes-on-shortest-paths grid distances end-node #{}))))))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))

