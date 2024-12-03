(ns day3.solution)


(def input (slurp "days/day3/input"))


(def part1 
  (->> input
    (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
    (map (fn [[_ left right]] (* (parse-long left) (parse-long right))))
    (reduce +)))


(def part2 
  (let [matcher (re-matcher #"(don't)|(do)|mul\((\d{1,3}),(\d{1,3})\)" input)]
    (loop [[_ dont do left right] (re-find matcher)
           enabled              true
           result               0] 
      (cond
        dont (recur (re-find matcher) false result)
        do   (recur (re-find matcher) true result)
        left (recur (re-find matcher) enabled 
               (if enabled 
                 (+ result (* (parse-long left) (parse-long right))) 
                 result))
        :else result))))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))