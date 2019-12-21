(ns aoc19.day10
  (:require [clojure.string :as string]))

(set! *unchecked-math* :warn-on-boxed)

(def input
  (->> (slurp "input/day10.txt")
       (string/split-lines)))

(def width (count (first input)))
(def height (count input))

(defn get-pos
  [^long index]
  (let [x (rem index ^long width)
        y (quot index ^long width)]
    [x y]))

(defn get-dir
  [^long base ^long index]
  (let [[^long bx ^long by] (get-pos base)
        [^long x ^long y] (get-pos index)
        x (- bx x)
        y (- by y)
        sum (+ (Math/abs x) (Math/abs y))]
    [(/ (- x) sum) (/ y sum)]))

(defn solve-1
  [xs]
  (let [indices (into [] (comp cat
                               (keep-indexed
                                (fn [i x]
                                  (when (= \# x)
                                    i))))
                      xs)]
    (->> (for [a indices
               b indices
               :when (not (== (long a) (long b)))]
           {:index a :dir (get-dir a b)})
         (group-by :index)
         (reduce-kv
          (fn [^long acc index dirs]
            (max acc (count (set dirs))))
          0))))

(comment

  (solve-1 input)

)

(defn get-distance
  [^long base ^long index]
  (let [bx (rem base ^long width)
        by (quot base ^long width)
        x (- bx (rem index ^long width))
        y (- by (quot index ^long width))]
    (+ (Math/abs x) (Math/abs y))))

(defn solve-2
  [xs]
  (let [indices (into [] (comp cat
                               (keep-indexed
                                (fn [i x]
                                  (when (= \# x)
                                    i))))
                      xs)]
    (->> (for [a indices
               b indices
               :when (not (== (long a) (long b)))]
           {:index a
            :pos (get-pos b)
            :dir (get-dir a b)
            :len (get-distance a b)})
         (group-by :index)
         (apply max-key (comp count #(into #{} (map :dir) %) val))
         (val)
         (map (fn [{:keys [dir len] :as data}]
                (let [[x y] dir
                      angle (Math/atan2 x y)]
                  (assoc data
                         :angle (if (neg? angle)
                                  (+ (* 2 Math/PI) angle)
                                  angle)
                         :len len))))
         (group-by :dir)
         (sort-by (comp :angle first val))
         (vals)
         (map #(sort-by :len %))
         (iterate #(keep next %))
         (take-while seq)
         (mapcat #(map first %))
         (drop 199)
         (first)
         (:pos)
         (map * [100 1])
         (apply +))))

(comment

  (time (solve-2 input))

)
