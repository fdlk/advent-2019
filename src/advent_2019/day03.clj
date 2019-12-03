(ns advent-2019.day03
  (:require [clojure.string :refer [split]])
  (:require [advent-2019.core :refer [lines parse-int sum]]))

(defn segment-end
  "Gets the endpoint for a segment starting in point"
  [point, segment]
  (let [direction (first segment)
        length (parse-int (subs segment 1))]
    (case direction
      \R (assoc point :x (+ (:x point) length))
      \L (assoc point :x (- (:x point) length))
      \U (assoc point :y (+ (:y point) length))
      \D (assoc point :y (- (:y point) length)))))

(def central-port {:x 0 :y 0})

(defn segments
  "Creates line segments for wire input"
  [wire]
  (->> wire
       (reductions segment-end central-port)
       (partition 2 1)))

(def input
  (->> "day03.txt"
       (lines)
       (map #(split %1 #","))
       (map segments)))

(def wire1 (first input))
(def wire2 (first (rest input)))

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
  "Determines if the line through s2 intersects with s1"
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
    {:x (:x (first s2)) :y (:y (first s1))}
    {:x (:x (first s1)) :y (:y (first s2))}))

(defn manhattan
  "Determines the manhattan distance between two points"
  [p1 p2]
  (+
   (Math/abs (- (:x p1) (:x p2)))
   (Math/abs (- (:y p1) (:y p2)))))

(def part1
  (reduce
   min
   (for [s1 wire1
         s2 wire2
         :when (intersect s1 s2)
         :let [crossing (intersection s1 s2)]]
     (manhattan crossing central-port))))

(def part2
  (reduce
   min
   (for [index1 (range (count wire1))
         index2 (range (count wire2))
         :let [s1 (nth wire1 index1)
               s2 (nth wire2 index2)]
         :when (intersect s1 s2)
         :let [crossing (intersection s1 s2)
               segs (concat (take index1 wire1)
                            [[(first s1) crossing]]
                            (take index2 wire2)
                            [[(first s2) crossing]])
               lengths (map #(apply manhattan %1) segs)]]
     (sum lengths))))

(defn -main
  [& args]
  (println part1 part2))