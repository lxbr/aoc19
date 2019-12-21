(ns aoc19.day09
  (:require [clojure.string :as string]))

(def input
  (-> (slurp "input/day09.txt")
      (string/trim)
      (string/split #",")
      (->> (mapv #(Long/parseLong %)))))

(def ops-spec
  {1 [:value :value :address]
   2 [:value :value :address]
   3 [:address]
   4 [:value]
   5 [:value :value]
   6 [:value :value]
   7 [:value :value :address]
   8 [:value :value :address]
   9 [:value]})

(defn get-args
  [op i base ops]
  (->> (iterate #(quot % 10) (quot op 100))
       (map #(rem % 10))
       (map (fn [j kind mode]
              (let [n (get ops j 0)]
                (case kind
                  :value (case mode
                           0 (get ops n 0)
                           1 n
                           2 (get ops (+ base n) 0))
                  :address (case mode
                             0 n
                             2 (+ base n)))))
            (range (inc i) Long/MAX_VALUE)
            (get ops-spec (rem op 100)))))

(defn interpret
  [in ops]
  (loop [i 0
         base 0
         ops ops
         in in
         out []]
    (let [op (get ops i)]
      (case (rem op 100)

        1 (let [[a b j] (get-args op i base ops)]
            (recur (+ 4 i) base (assoc ops j (+ a b)) in out))

        2 (let [[a b j] (get-args op i base ops)]
            (recur (+ 4 i) base (assoc ops j (* a b)) in out))

        3 (let [[j] (get-args op i base ops)]
            (recur (+ 2 i) base (assoc ops j (first in)) (next in) out))

        4 (let [[n] (get-args op i base ops)]
            (recur (+ 2 i) base ops in (conj out n)))

        5 (let [[a j] (get-args op i base ops)]
            (recur (if-not (zero? a) j (+ 3 i)) base ops in out))

        6 (let [[a j] (get-args op i base ops)]
            (recur (if (zero? a) j (+ 3 i)) base ops in out))

        7 (let [[a b j] (get-args op i base ops)]
            (recur (+ 4 i) base (assoc ops j (if (< a b) 1 0)) in out))

        8 (let [[a b j] (get-args op i base ops)]
            (recur (+ 4 i) base (assoc ops j (if (== a b) 1 0)) in out))

        9 (let [[offset] (get-args op i base ops)]
            (recur (+ 2 i) (+ base offset) ops in out))

        99 out))))

(defn solve-1
  [input]
  (interpret [1] (into {} (map-indexed vector) input)))

(comment

  (solve-1 input)

  )

(defn solve-2
  [input]
  (interpret [2] (into {} (map-indexed vector) input)))

(comment

  (solve-2 input)

  )
