(ns advent-2019.day06
  (:require [advent-2019.core :refer [sum lines]])
  (:require [clojure.string :refer [split]]))

(def input
  (->> "day06.txt"
       (lines)
       (map #(split % #"\)"))
       (reduce (fn [coll [v k]] (assoc coll k v)) {})))

(defn orbits-to-center
  [object-id]
  (->> (iterate input object-id)
       (take-while #(not= % "COM"))))

(def part1
  (->> (keys input)
       (map orbits-to-center)
       (map count)
       (sum)))

(def part2
  (->> (concat (orbits-to-center "YOU") (orbits-to-center "SAN"))
       (frequencies)
       (filter #(= (second %) 1))
       (count)
       (+ -2)))

(defn -main [& args]
  (println part1 part2))