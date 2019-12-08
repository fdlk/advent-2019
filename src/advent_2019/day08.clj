(ns advent-2019.day08
  (:require [advent-2019.core :refer [parse-int sum lines]]))

(def input
  (->> "day08.txt"
       (lines)
       (first)
       (map int)
       (map #(- % (int \0)))
       (partition (* 25 6))))

(def part1 (->> input
                (map frequencies)
                (sort-by #(get % 0))
                (first)))

(defn -main [& args]
  (println (* (get part1 1) (get part1 2))))