(ns advent-2019.day10
  (:require [clojure.math.numeric-tower :refer [expt]])
  (:require [advent-2019.core :refer [lines]])
  (:require [clojure.string :refer [join]])
  (:require [clojure.set :refer [difference]]))

(def input (->> "day10.txt"
                (lines)
                (map vec)
                (vec)))

(def asteroids
  (set (for [y (range (count input))
             x (range (count (first input)))
             :when (= (get-in input [y x]) \#)]
         [x y])))

(defn inner-product
  "Determines the inner product of two vectors"
  [[xa ya] [xb yb]]
  (+ (* xa xb) (* ya yb)))

(defn codirectional
  "Determines if two vectors are codirectional and aligned"
  [a b]
  (and
   (pos-int? (inner-product a b))
   (= (expt (inner-product a b) 2)
      (* (inner-product a a)
         (inner-product b b)))))

(defn obstructs
  "Determines if b obstructs a's view of c"
  [a b c]
  (let [ab (map - b a) ac (map - c a)]
    (and
     (codirectional ab ac)
     (< (inner-product ab ab) (inner-product ac ac)))))

(defn can-see-from
  "Determines if asteroid a can be seen from b"
  [a c]
  (not-any?
   (fn [b] (obstructs a b c))
   (difference asteroids #{a c})))

(defn visible-asteroids
  "The asteroids visible from a"
  [a]
  (filter (partial can-see-from a) (disj asteroids a)))

(defn number-of-visible-asteroids
  "The number of asteroids visible from a"
  [a]
  (count (visible-asteroids a)))

(def part1 (apply max-key first (map (fn [a] [(number-of-visible-asteroids a) a]) asteroids)))

(def part2
  (let [[_ [x0 y0]] part1]
    (nth (sort-by
          (fn [[x y]] (- (Math/atan2  (- x x0) (- y y0))))
          (visible-asteroids [x0 y0]))
         (dec 200))))

(defn -main [& args] (println part1 part2))