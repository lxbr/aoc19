(ns aoc19.day04
  (:require [clojure.string :as string]))

(def input
  (-> (slurp "input/day04.txt")
      (string/trim)
      (string/split #"-")
      (->> (mapv #(Long/parseLong %)))))

(defn solve-1
  [[from to]]
  (->> (range from (inc to))
       (map str)
       (filter (fn [s]
                 (and (== 6 (count s))
                      (->> (partition 2 1 s)
                           (some #(apply = %)))
                      (= (seq s) (sort s)))))
       (count)))

(comment
  
  (solve-1 input)

)

(defn solve-2
  [[from to]]
  (->> (range from (inc to))
       (map str)
       (filter (fn [s]
                 (let [partitions (partition-by identity s)]
                   (and (== 6 (count s))
                        (->> partitions
                             (some #(== 2 (count %))))
                        (= (seq s) (sort s))))))
       (count)))

(comment
  
  (solve-2 input)

)
