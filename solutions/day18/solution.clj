(ns day18.solution
  (:require 
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day18/input"
    slurp
    string/trim 
    string/split-lines))


(def ^:private directions [[1 0] [0 1] [-1 0] [0 -1]])


(defn- parse-input [input] 
  (map (fn [line] (mapv parse-long (string/split line #"," 2))) input))


(defn- dfs [grid width height]
  (loop [queue 	   [[0 0]]
         distances {[0 0] 0}]
    (let [[[x y]] queue]
      (if (or (nil? x) (= [x y] [(dec width) (dec height)]))
        (get distances [(dec width) (dec height)])
        (let [[queue distances] (reduce
                                  (fn [[queue distances] [dx dy]]
                                    (let [x1 (+ x dx) y1 (+ y dy)]
                                      (if
                                        (or
                                          (neg? x1)
                                          (neg? y1)
                                          (>= x1 width)
                                          (>= y1 height)
                                          (contains? grid [x1 y1])
                                          (and (contains? distances [x1 y1]) (>= (inc (get distances [x y])) (get distances [x1 y1]))))
                                        [queue distances]
                                        [(conj queue [x1 y1]) (assoc distances [x1 y1] (inc (get distances [x y])))])))
                                  [(vec (rest queue)) distances]
                                  directions)]
          (recur queue distances))))))


(defn part1
  ([input] 
   (part1 input 71 71 1024))
  ([input width height amount] 
   (dfs (set (take amount (parse-input input))) width height)))


(defn part2
  ([input] 
   (part2 input 71 71 1024))
  ([input width height min-amount]
   (let [input (vec (parse-input input))]
     (loop [grid (set (take min-amount input)) i min-amount]
       (if (nil? (dfs grid width height))
         (string/join "," (get input (dec i)))
         (recur (conj grid (get input i)) (inc i)))))))


(defn -main []
  (println "part1 " (part1 input))
  (println "part2 " (part2 input)))