(ns aoc19.day03
  (:require [clojure.string :as string]))

(def input
  (for [line (string/split-lines (slurp "input/day03.txt"))]
    (for [op (string/split line #",")
          :let [length (Long/parseLong (subs op 1))]]
      (case (first op)
        \D {:dir :down  :len length}
        \U {:dir :up    :len length}
        \L {:dir :left  :len length}
        \R {:dir :right :len length}))))

(defn vertical?
  [{:keys [dir]}]
  (contains? #{:up :down} dir))

(defn intersection
  [vertical-line horizontal-line]
  (let [{[a b] :from
         [c d] :to} vertical-line
        {[e f] :from
         [g h] :to} horizontal-line]
    (when (and (or (<= e a g)
                   (<= g a e))
               (or (<= b f d)
                   (<= d f b)))
      [a f])))

(defn solve-1
  [paths]
  (let [[a b] (for [path paths]
                (->> (reductions
                      (fn [[x y] {:keys [dir len]}]
                        (case dir
                          :up    [x (+ y len)]
                          :down  [x (- y len)]
                          :left  [(- x len) y]
                          :right [(+ x len) y]))
                      [0 0]
                      path)
                     (partition 2 1)
                     (map (fn [data [from to]]
                            (assoc data :from from :to to))
                          path)))]
    (->> (for [a-line a
               b-line b]
           (cond
             (and (vertical? a-line)
                  (not (vertical? b-line)))
             (intersection a-line b-line)
             (and (vertical? b-line)
                  (not (vertical? a-line)))
             (intersection b-line a-line)))
         (remove nil?)
         (map (fn [xs] (reduce + 0 (map #(Math/abs %) xs))))
         (sort)
         (first))))

(comment
  
  (solve-1 input)

)

(defn intersection-2
  [vertical-line horizontal-line]
  (let [{[a b] :from
         [c d] :to
         steps-1 :steps-taken
         vdir :dir} vertical-line
        {[e f] :from
         [g h] :to
         steps-2 :steps-taken
         hdir :dir} horizontal-line]
    (when (and (or (<= e a g)
                   (<= g a e))
               (or (<= b f d)
                   (<= d f b)))
      (+ steps-1 steps-2
         (if (= :up vdir)
           (- f b)
           (- b f))
         (if (= :right hdir)
           (- a e)
           (- e a))))))

(defn solve-2
  [paths]
  (let [[a b] (for [path paths]
                (->> (reductions
                      (fn [[x y] {:keys [dir len]}]
                        (case dir
                          :up    [x (+ y len)]
                          :down  [x (- y len)]
                          :left  [(- x len) y]
                          :right [(+ x len) y]))
                      [0 0]
                      path)
                     (partition 2 1)
                     (map (fn [data steps-taken [from to]]
                            (assoc data :from from :to to :steps-taken steps-taken))
                          path
                          (reductions + 0 (map :len path)))))]
    (->> (for [a-line (next a)
               b-line (next b)]
           (cond
             (and (vertical? a-line)
                  (not (vertical? b-line)))
             (intersection-2 a-line b-line)
             (and (vertical? b-line)
                  (not (vertical? a-line)))
             (intersection-2 b-line a-line)))
         (remove nil?)
         (sort)
         (first))))

(comment

  (solve-2 input)

)
