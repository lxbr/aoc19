(ns aoc19.day08
  (:require [clojure.string :as string]))

(def input
  (-> (slurp "input/day08.txt")
      (string/trim)))

(def width 25)
(def height 6)

(defn solve-1
  [xs]
  (let [{ones \1
         twos \2} (->> (partition (* width height) xs)
                       (apply min-key #(count (filter #{\0} %)))
                       (group-by identity))]
    (* (count ones) (count twos))))

(comment
  
  (solve-1 input)

)

(defn solve-2
  [xs]
  (->> (partition (* width height) xs)
       (apply map (fn [& pixels]
                    (some #{\0 \1} pixels)))
       (replace {\0 \space \1 "\u2588"})
       (partition width)
       (map #(apply str %))))

(comment

  (run! println (solve-2 input))

)
