(ns advent-2019.day08
  (:require [advent-2019.core :refer [lines]])
  (:require [clojure.string :refer [join]]))

(def layers
  (->> "day08.txt"
       (lines)
       (first)
       (map int)
       (map #(- % (int \0)))
       (partition (* 25 6))))

(def part1 (->> layers
                (map frequencies)
                (sort-by #(get % 0))
                (first)))

(defn pixel [values]
  (->> values
       (filter #(not= 2 %))
       (first)))

(def part2
  (join \newline
        (->> layers
             (apply interleave)
             (partition (count input))
             (map pixel)
             (map #(if (= % 1) "#" " "))
             (partition 25)
             (map join))))

(defn -main [& args]
  (println (* (get part1 1) (get part1 2)))
  (println part2))