(ns advent-2019.day04   (:require [clojure.string :refer [join]]))

(def input (range 402328 (inc 864247)))
(def digit-counts
  (->> input
       (map str)
       (filter (fn is-monotonous [password] (= password (join (sort password)))))
       (map frequencies)
       (map vals)
       (map set)))
(def part1 (count (filter (fn [counts] (not= counts #{1})) digit-counts)))
(def part2 (count (filter (fn [counts] (contains? counts 2)) digit-counts)))

(defn -main [& args] (println part1 part2))