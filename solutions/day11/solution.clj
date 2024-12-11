(ns day11.solution
  (:require 
   [clojure.string :as string]))


(def ^:private input
  (-> "solutions/day11/input"
    slurp
    string/trim))


(def ^:private DIGITS_LOG10 
  [0  0  0  1  1  1  2  2  2  3
   3  3  3  4  4  4  5  5  5  6
   6  6  6  7  7  7  8  8  8  9
   9  9  9 10 10 10 11 11 11 12
   12 12 12 13 13 13 14 14 14 15
   15 15 15 16 16 16 17 17 17 18
   18 18 18])


(def ^:private POWERS_OF_10 
  (vec (take 19 (iterate #(* % 10) 1))))


(defn ilog10 
  "https://stackoverflow.com/q/55032982/11071180"
  [n]
  {:pre [(<= 1 n Long/MAX_VALUE)]}
  (let [lz (bit-xor 63 (Long/numberOfLeadingZeros n))
        guess (get DIGITS_LOG10 lz)]
    (if (< n (get POWERS_OF_10 guess)) (dec guess) guess)))


(defn- ipow10 [n]
  {:pre [(<= 0 n 18)]}
  (get POWERS_OF_10 n))


(defn- count-digits [n]
  (if (zero? n) 1 (inc (ilog10 n))))


(defn- simulate [stones]
  (reduce
    (partial merge-with +)
    {}
    (map
      (fn [[n count]]
        (if (zero? n)
          {1 count}
          (let [num-digits (count-digits n)]
            (if (even? num-digits)
              (let [pow10 (ipow10 (/ num-digits 2))
                    a (quot n pow10)
                    b (mod n pow10)]
                (if (= a b)
                  {a (* count 2)}
                  {a count b count}))
              {(* 2024 n) count}))))
      stones)))


(defn- run [input count]
  (let [stones (map parse-long (string/split input #" "))
        stones (reduce
                 #(merge-with + %1 {%2 1})
                 {}
                 stones)]
    (reduce + (map last (nth (iterate simulate stones) count)))))


(def^:private part1 
  (run input 25))


(def^:private part2 
  (run input 75))


(defn -main []
  (println "part1 " part1)
  (println "part2 " part2))