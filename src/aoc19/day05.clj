(ns aoc19.day05
  (:require [clojure.string :as string]))

(def input
  (-> (slurp "input/day05.txt")
      (string/trim)
      (string/split #",")
      (->> (mapv #(Long/parseLong %)))))

(defn solve-1
  [in xs]
  (loop [i 0
         result xs
         in in
         out []]
    (let [op (nth result i)
          param-modes (->> (iterate #(quot % 10) (quot op 100))
                           (map #(rem % 10)))]
      (case (rem op 100)
        (1 2) (let [[j k l] (subvec result (inc i))
                    [jm km] param-modes
                    a (if (zero? jm) (nth result j) j)
                    b (if (zero? km) (nth result k) k)
                    c (if (== 1 (rem op 100)) (+ a b) (* a b))]
                (recur (+ 4 i) (assoc result l c) in out))
        3 (let [j (nth result (inc i))]
            (recur (+ 2 i) (assoc result j (first in)) (next in) out))
        4 (let [j (nth result (inc i))
                n (if (zero? (first param-modes))
                    (nth result j)
                    j)]
            (recur (+ 2 i) result in (conj out n)))
        99 out))))

(comment

  (solve-1 [1] input)

)

(defn solve-2
  [in xs]
  (loop [i 0
         result xs
         in in
         out []]
    (let [op (nth result i)
          param-modes (->> (iterate #(quot % 10) (quot op 100))
                           (map #(rem % 10)))]
      (case (rem op 100)
        (1 2) (let [[j k l] (subvec result (inc i))
                    [jm km] param-modes
                    a (if (zero? jm) (nth result j) j)
                    b (if (zero? km) (nth result k) k)
                    c (if (== 1 (rem op 100)) (+ a b) (* a b))]
                (recur (+ 4 i) (assoc result l c) in out))
        3 (let [j (nth result (inc i))]
            (recur (+ 2 i) (assoc result j (first in)) (next in) out))
        4 (let [j (nth result (inc i))
                n (if (zero? (first param-modes))
                    (nth result j)
                    j)]
            (recur (+ 2 i) result in (conj out n)))
        5 (let [[j k] (subvec result (inc i))
                [jm km] param-modes
                test (if (zero? jm)
                       (nth result j)
                       j)]
            (if-not (zero? test)
              (recur (if (zero? km) (nth result k) k) result in out)
              (recur (+ 3 i) result in out)))
        6 (let [[j k] (subvec result (inc i))
                [jm km] param-modes
                test (if (zero? jm)
                       (nth result j)
                       j)]
            (if (zero? test)
              (recur (if (zero? km) (nth result k) k) result in out)
              (recur (+ 3 i) result in out)))
        7 (let [[j k l] (subvec result (inc i))
                [jm km] param-modes
                jv (if (zero? jm) (nth result j) j)
                kv (if (zero? km) (nth result k) k)]
            (recur (+ 4 i) (assoc result l (if (< jv kv) 1 0)) in out))
        8 (let [[j k l] (subvec result (inc i))
                [jm km] param-modes
                jv (if (zero? jm) (nth result j) j)
                kv (if (zero? km) (nth result k) k)]
            (recur (+ 4 i) (assoc result l (if (== jv kv) 1 0)) in out))
        99 out))))

(comment
  
  (solve-2 [5] input)

)
