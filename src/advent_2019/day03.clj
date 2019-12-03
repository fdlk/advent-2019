(ns advent-2019.day03
  (:require [clojure.string :refer [split]])
  (:require [advent-2019.core :refer [lines parse-int]]))

(defn parse-line
  "Parses input line"
  [line]
  (split line #","))

(defn reducer
  "Reducer for the segments"
  [point, segment]
  (let [direction (first segment)
        length (parse-int (subs segment 1))]
    (case direction
      \R (assoc point :x (+ (:x point) length))
      \L (assoc point :x (- (:x point) length))
      \U (assoc point :y (+ (:y point) length))
      \D (assoc point :y (- (:y point) length)))))

(defn segments
  "Creates line segments for wire input"
  [wire]
  (partition 2 1 (reductions reducer {:x 0 :y 0} wire)))

(def input
  (->> "day03.txt"
       (lines)
       (map parse-line)
       (map segments)))

(defn contains
  "Determines if a range of numbers strictly contains a number"
  [range number]
  (let [[from, to] (sort range)]
    (and (< from number) (> to number))))

(defn is-horizontal
  "Determines if a segment is horizontal"
  [[from, to]]
  (= (:y from) (:y to)))

(defn is-vertical
  "Determines if a segment is vertical"
  [[from, to]]
  (= (:x from) (:x to)))

(defn intersect-internal
  "Determines if the line through s2 lies within s1"
  [s1, s2]
  (or
   (and
    (is-vertical s1)
    (is-horizontal s2)
    (contains (map :y s1) (:y (first s2))))
   (and
    (is-horizontal s1)
    (is-vertical s2)
    (contains (map :x s1) (:x (first s2))))))

(defn intersect
  "Determines if two segments intersect"
  [s1, s2]
  (and
   (intersect-internal s1 s2)
   (intersect-internal s2 s1)))

(defn intersection
  "Determines the intersection of two segments, given that they intersect"
  [s1, s2]
  (if (is-horizontal s1)
    {:x (:x (first s2)) :y (:y (first s1))})
  {:x (:x (first s1)) :y (:y (first s2))})

(defn manhattan
  "Determines the manhattan distance to the central port"
  [p]
  (+ (Math/abs (:x p)) (Math/abs (:y p))))

(defn -main
  [& args]
  (let [[wire1 wire2] input]
    (println
     (reduce min (for [s1 wire1
                       s2 wire2
                       :when (intersect s1 s2)
                       :let [crossing (intersection s1 s2)]]
                   (manhattan crossing))))))