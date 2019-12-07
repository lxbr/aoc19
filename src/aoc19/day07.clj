(ns aoc19.day07
  (:require [clojure.string :as string]))

(def input
  (-> (slurp "input/day07.txt")
      (string/trim)
      (string/split #",")
      (->> (mapv #(Long/parseLong %)))))

(defn f
  ([in xs] (f {:i 0 :state xs :in in}))
  ([{:keys [i state in]}]
   (loop [i i
          state state
          in in
          out []]
     (let [op (nth state i)
           param-modes (->> (iterate #(quot % 10) (quot op 100))
                            (map #(rem % 10)))]
       (case (rem op 100)
         (1 2) (let [[j k l] (subvec state (inc i))
                     [jm km] param-modes
                     a (if (zero? jm) (nth state j) j)
                     b (if (zero? km) (nth state k) k)
                     c (if (== 1 (rem op 100)) (+ a b) (* a b))]
                 (recur (+ 4 i) (assoc state l c) in out))
         3 (let [j (nth state (inc i))]
             (if (seq in)
               (recur (+ 2 i) (assoc state j (first in)) (next in) out)
               {:i i
                :state state
                :out out}))
         4 (let [j (nth state (inc i))
                 n (if (zero? (first param-modes))
                     (nth state j)
                     j)]
             (recur (+ 2 i) state in (conj out n)))
         5 (let [[j k] (subvec state (inc i))
                 [jm km] param-modes
                 test (if (zero? jm)
                        (nth state j)
                        j)]
             (if-not (zero? test)
               (recur (if (zero? km) (nth state k) k) state in out)
               (recur (+ 3 i) state in out)))
         6 (let [[j k] (subvec state (inc i))
                 [jm km] param-modes
                 test (if (zero? jm)
                        (nth state j)
                        j)]
             (if (zero? test)
               (recur (if (zero? km) (nth state k) k) state in out)
               (recur (+ 3 i) state in out)))
         7 (let [[j k l] (subvec state (inc i))
                 [jm km] param-modes
                 jv (if (zero? jm) (nth state j) j)
                 kv (if (zero? km) (nth state k) k)]
             (recur (+ 4 i) (assoc state l (if (< jv kv) 1 0)) in out))
         8 (let [[j k l] (subvec state (inc i))
                 [jm km] param-modes
                 jv (if (zero? jm) (nth state j) j)
                 kv (if (zero? km) (nth state k) k)]
             (recur (+ 4 i) (assoc state l (if (== jv kv) 1 0)) in out))
         99 {:out out})))))

(defn solve-1
  [ops]
  (let [xs (set (range 5))]
    (->> (for [i xs
               j (disj xs i)
               k (disj xs i j)
               l (disj xs i j k)
               m (disj xs i j k l)]
           (let [{[a] :out} (f [i 0] ops)
                 {[b] :out} (f [j a] ops)
                 {[c] :out} (f [k b] ops)
                 {[d] :out} (f [l c] ops)
                 {[e] :out} (f [m d] ops)]
             e))
         (reduce max 0))))

(comment

  (solve-1 input)

)

(defn solve-2
  [ops]
  (let [xs (set (range 5 10))]
    (->> (for [i xs
               j (disj xs i)
               k (disj xs i j)
               l (disj xs i j k)
               m (disj xs i j k l)]
           (loop [state-a {:i 0 :state ops :in [i 0]}
                  state-b {:i 0 :state ops :in [j]}
                  state-c {:i 0 :state ops :in [k]}
                  state-d {:i 0 :state ops :in [l]}
                  state-e {:i 0 :state ops :in [m]}]
             (let [{[a] :out :as state-a} (f state-a)
                   {[b] :out :as state-b} (f (update state-b :in conj a))
                   {[c] :out :as state-c} (f (update state-c :in conj b))
                   {[d] :out :as state-d} (f (update state-d :in conj c))
                   {[e] :out :as state-e} (f (update state-e :in conj d))]
               state-e
               (if (:state state-e)
                 (recur (assoc state-a :in [e])
                        state-b
                        state-c
                        state-d
                        state-e)
                 e))))
         (reduce max 0))))

(comment

  (solve-2 input)

)
