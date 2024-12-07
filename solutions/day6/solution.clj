(ns day6.solution
  (:require
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day6/input"
    slurp
    string/trim
    string/split-lines))


(def ^:private read-map
  "Read file to 2D matrix of symbols"
  (mapv vec input))


(def ^:private caret-direction
  "Mapping from caret symbol to vector direction"
  {\^ [0 -1] 
   \> [1 0] 
   \v [0 1] 
   \< [-1 0]})


(defn- add-vec
  "Add two vectors"
  [v1 v2]
  (mapv + v1 v2))


(defn- get-2d
  "Get m[x][y]"
  [m [x y]]
  (get-in m [y x]))


(defn- set-2d
  "Set m[x][y] = val"
  [m [x y] val]
  (assoc-in m [y x] val))


(defn- find-guard
  "Find the caret in the matrix and return its position and direction."
  [m]
  (first (for [y     (range (count m))
               x     (range (count (m 0)))
               :let  [dir (caret-direction (get-2d m [x y]))]
               :when dir]
           {:position  [x y] 
            :direction dir})))


(defn- rotate
  "Rotate guard to the right"
  [guard]
  (case (:direction guard)
    [0 -1] (assoc guard :direction [1 0])
    [1 0]  (assoc guard :direction [0 1])
    [0 1]  (assoc guard :direction [-1 0])
    [-1 0] (assoc guard :direction [0 -1])))


(defn- step
  "Make guard step forward"
  [guard]
  (update guard :position add-vec (:direction guard)))


(defn- run
  "Run guard from staring position `guard`, in given matrix `m`
   Returns set of visited guard positions (alongside directions)
   or nil if there is a loop"
  [m visited guard]
  (let [next-position (add-vec (:position guard) (:direction guard))
        next-char     (get-2d m next-position)
        new-visited   (conj visited guard)]
    (cond (contains? visited guard) nil ; loop found
      (= next-char nil) new-visited ; end of matrix, guard finished
      (= next-char \#)  (recur m new-visited (rotate guard)) ; rotate
      :else             (recur m new-visited (step guard))))) ; just go forward


(def part1 
  (->> (run read-map #{} (find-guard read-map))
    (map :position)
    set
    count))


(def part2
  (let [guard     (find-guard read-map)
        obstacles (disj (set (map :position (run read-map #{} guard))) (:position guard))]
    (->> obstacles
      ; remain only those obstacles, that generate loop (run returns nil)
      (remove #(run (set-2d read-map % \#) #{} guard))
      count)))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))
