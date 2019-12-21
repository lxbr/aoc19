(ns aoc19.day11
  (:require [clojure.string :as string]))

(def input
  (-> (slurp "input/day11.txt")
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
  ([in ops]
   (interpret {:i 0 :base 0 :in in :ops ops}))
  ([{:keys [i base in ops]}]
   (loop [i i
          base base
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
             (if (seq in)
               (recur (+ 2 i) base (assoc ops j (first in)) (next in) out)
               {:i i
                :base base
                :ops ops
                :out out}))

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

         99 out)))))

(defn solve-1
  [xs]
  (->> {:pos [0 0]
        :dir :up
        :visited {}
        :state {:i 0
                :base 0
                :ops xs}
        :done false}
       (iterate (fn [{:keys [pos dir visited state] :as data}]
                  (let [in (if (even? (get visited pos 0))
                             [0]
                             [1])
                        {[color turn] :out
                         :as new-state} (interpret (assoc state :in in))]
                    (if (some? color)
                      (let [new-dir (case dir
                                      :up (case turn 0 :left 1 :right)
                                      :down (case turn 0 :right 1 :left)
                                      :left (case turn 0 :down 1 :up)
                                      :right (case turn 0 :up 1 :down))]
                        {:pos (vec (map + pos (case new-dir
                                                :up [0 1]
                                                :down [0 -1]
                                                :left [-1 0]
                                                :right [1 0])))
                         :dir new-dir
                         :visited (assoc visited pos color)
                         :state new-state
                         :done false})
                      (assoc data :done true)))))
       (drop-while (complement :done))
       (first)
       (:visited)
       (count)))

(comment

  (solve-1 input)

)

(defn solve-2
  [xs]
  (let [{:keys [visited]}
        (->> {:pos [0 0]
              :dir :up
              :visited {[0 0] 1}
              :state {:i 0
                      :base 0
                      :ops (into {} (map-indexed vector) xs)}
              :done false}
             (iterate (fn [{:keys [pos dir visited state] :as data}]
                        (let [in (if (even? (get visited pos 0))
                                   [0]
                                   [1])
                              {[color turn] :out
                               :as new-state} (interpret (assoc state :in in))]
                          (if (some? color)
                            (let [new-dir (case dir
                                            :up (case turn 0 :left 1 :right)
                                            :down (case turn 0 :right 1 :left)
                                            :left (case turn 0 :down 1 :up)
                                            :right (case turn 0 :up 1 :down))]
                              {:pos (vec (map + pos (case new-dir
                                                      :up [0 1]
                                                      :down [0 -1]
                                                      :left [-1 0]
                                                      :right [1 0])))
                               :dir new-dir
                               :visited (assoc visited pos color)
                               :state new-state
                               :done false})
                            (assoc data :done true)))))
             (drop-while (complement :done))
             (first))
        x-min (apply min (map first (keys visited)))
        x-max (apply max (map first (keys visited)))
        y-min (apply min (map second (keys visited)))
        y-max (apply max (map second (keys visited)))]
    (->> (for [y (range y-min (inc y-max))]
           (->> (for [x (range x-min (inc x-max))]
                  (case (get visited [x y] 0)
                    0 \space
                    1 \#))
                (apply str)))
         (reverse)
         (run! println))))

(comment

  (solve-2 input)

)
