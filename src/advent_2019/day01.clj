(ns advent-2019.day01
  (:require [clojure.string :refer [split-lines]])
  (:require [advent-2019.core :refer [parse-int]]))

(def input
  (map parse-int (split-lines (slurp "resources/day01.txt"))))

(defn fuel
  [mass]
  "Determines how much fuel is needed to launch a given mass"
  (- (quot mass 3) 2))

(def part1
  (reduce + (map fuel input)))

(defn total-fuel
  [mass]
  "Determines how much fuel is needed to launch a given mass plus its fuel"
  (reduce + (take-while pos? (rest (iterate fuel mass)))))

(def part2
  (reduce + (map total-fuel input)))

(defn -main
  "Call the method of the day"
  [& args]
  (println [part1, part2]))