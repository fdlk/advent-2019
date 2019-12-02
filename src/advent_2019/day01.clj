(ns advent-2019.day01
  (:require [advent-2019.core :refer [parse-int sum lines]]))

(def input
  (->> "day01.txt"
    (lines)
    (map parse-int)))

(defn fuel
  [mass]
  "Determines how much fuel is needed to launch a given mass"
  (-> mass
    (quot 3)
    (- 2)))

(def part1
  (->> input
    (map fuel)
    (sum)))

(defn total-fuel
  [mass]
  "Determines how much fuel is needed to launch a given mass plus its fuel"
  (->> mass
    (iterate fuel)
    (rest)
    (take-while pos?)
    (sum)))

(def part2
  (->> input
    (map total-fuel)
    (sum)))

(defn -main
  [& args]
  (println [part1, part2]))