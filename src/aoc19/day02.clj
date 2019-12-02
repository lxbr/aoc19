(ns aoc19.day02
  (:require [clojure.string :as string]))

(def input
  (-> (slurp "input/day02.txt")
      (string/trim)
      (string/split #",")
      (->> (mapv #(Long/parseLong %)))))

(defn solve-1
  [xs]
  (loop [i 0
         result xs]
    (case (nth result i)
      1 (let [[j k l] (subvec result (inc i))
              a (nth result j)
              b (nth result k)]
          (recur (+ 4 i) (assoc result l (+ a b))))
      2 (let [[j k l] (subvec result (inc i))
              a (nth result j)
              b (nth result k)]
          (recur (+ 4 i) (assoc result l (* a b))))
      99 (first result))))

(comment

  (solve-1 (assoc input 1 12 2 2))

)

(def expected 19690720)

(defn solve-2
  [xs]
  (->> (for [noun (range 100)
             verb (range 100)]
         [noun verb (solve-1 (assoc xs 1 noun 2 verb))])
       (drop-while (fn [[_ _ result]]
                     (not= expected result)))
       (map (fn [[noun verb]]
              (+ (* 100 noun) verb)))
       (first)))

(comment
  
  (solve-2 input)

)
