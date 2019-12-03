(ns advent-2019.day03
  (:require [clojure.string :refer [split join]])
  (:require [clojure.set :refer [intersection]])
  (:require [advent-2019.core :refer [lines parse-int manhattan]]))

(defn point-in-direction [[x y] distance direction]
  (case direction
    \R [(+ x distance) y]
    \L [(- x distance) y]
    \U [x (+ y distance)]
    \D [x (- y distance)]))

(defn move [start instruction]
  (let [[direction & length-chars] instruction
        length (parse-int (join length-chars))]
    [(point-in-direction start length direction)
     (map #(point-in-direction start (inc %) direction) (range length))]))

(defn follow-instructions
  [instructions]
  (loop [pos [0 0] points [] leftover instructions]
    (if (empty? leftover)
      points
      (let [instruction (first leftover)
            [end new-points] (move pos instruction)]
        (recur end (concat points new-points) (rest leftover))))))

(def input
  (->> "day03.txt"
       (lines)
       (map #(split % #","))
       (map follow-instructions)))

(def wire1 (first input))
(def wire2 (first (rest input)))
(def intersections (intersection (set wire1) (set wire2)))

(def part1
  (->> intersections
       (map #(manhattan % [0 0]))
       (reduce min)))

(def part2
  (->> intersections
       (map #(+ 2 (.indexOf wire1 %) (.indexOf wire2 %)))
       (reduce min)))

(defn -main
  [& args]
  (println part1 part2))