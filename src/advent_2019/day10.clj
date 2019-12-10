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
  "Determines if two vectors are codirectinal"
  [a b]
  (and
    (pos-int? (inner-product a b))
    (= (expt (inner-product a b) 2)
      (* (inner-product a a)
          (inner-product b b)))))

(defn vec-sub
  "Subtracts two vectors"
  [a b]
  (map - a b))

(defn obstructs
  "Determines if b obstructs a's view of c"
  [a b c]
  (let [ab (vec-sub b a) ac (vec-sub c a)]
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
 (filter (fn [c] (can-see-from a c)) (disj asteroids a)))

(defn number-of-visible-asteroids
  "The number of asteroids visible from a"
  [a]
  (count (visible-asteroids a)))

(def part1 (reduce max (map number-of-visible-asteroids asteroids)))

(defn -main [& args] (println part1))