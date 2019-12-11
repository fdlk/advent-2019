(ns advent-2019.day10
  (:require [advent-2019.core :refer [lines, manhattan]]))

(def input (->> "day10.txt"
                (lines)
                (map vec)
                (vec)))

(def asteroids
  (set (for [y (range (count input))
             x (range (count (first input)))
             :when (= (get-in input [y x]) \#)]
         [x y])))

(defn angle
  "The angle of vector from x0 y0 to x1 y1, range is [-Pi,Pi), cutoff when the vector points up, i.e. (0,-1), angle increasing clockwise"
  [[x0 y0] [x1 y1]] (- (Math/atan2 (- x1 x0) (- y1 y0))))

(defn number-of-visible-asteroids [a]
  (->> (disj asteroids a)
       (map (partial angle a))
       (distinct)
       (count)))

(def base (apply max-key number-of-visible-asteroids asteroids))
(def part1  (number-of-visible-asteroids base))

(def part2
  (let [others (disj asteroids base)
        groups (group-by (partial angle base) others)
        sorted-groups (sort-by first groups)
        group (second (nth sorted-groups (dec 200)))]
    (apply min-key (partial manhattan base) group)))

(defn -main [& args] (println part1 part2))