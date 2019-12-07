(ns aoc19.day06
  (:require [clojure.string :as string]))

(def input
  (->> (slurp "input/day06.txt")
       (string/split-lines)
       (map #(zipmap [:parent :child] (string/split % #"\)")))
       (group-by :parent)))

(defn solve-1
  [index]
  (loop [level 0
         nodes ["COM"]
         result 0]
    (if (seq nodes)
      (recur (inc level)
             (for [node nodes
                   {:keys [child]} (get index node)]
               child)
             (+ result (* (count nodes) level)))
      result)))

(comment
  
  (solve-1 input)

)

(defn solve-2
  [index]
  (let [inverted (->> (for [[parent data] input
                            {:keys [child]} data]
                        [child parent])
                      (into {}))
        a (->> (iterate inverted "SAN")
               (into [] (take-while some?)))
        b (->> (iterate inverted "YOU")
               (into {} (comp (take-while some?)
                              (map-indexed (fn [i s] [s i])))))]
    (reduce-kv
     (fn [_ i s]
       (when-some [j (get b s)]
         (reduced (+ i j -2))))
     nil
     a)))

(comment

  (solve-2 input)

)
