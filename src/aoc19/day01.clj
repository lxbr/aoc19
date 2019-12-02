(ns aoc19.day01
  (:require [clojure.string :as string]))

(def input
  (->> (slurp "input/day01.txt")
       (string/split-lines)
       (mapv #(Long/parseLong %))))

(defn solve-1
  [xs]
  (transduce (map #(- (quot % 3) 2)) + 0 input))

(comment
  
  (solve-1 input)

)

(defn solve-2
  [xs]
  (->> (for [x xs]
         (->> (iterate #(- (quot % 3) 2) x)
              (next)
              (take-while pos?)
              (reduce + 0)))
       (reduce + 0)))

(comment

  (solve-2 input)

)
