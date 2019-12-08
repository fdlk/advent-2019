(ns advent-2019.day08
  (:require [advent-2019.core :refer [parse-int sum lines]]))

(def input
  (->> "day08.txt"
       (lines)
       (first)
       (map int)
       (map #(- % (int \0)))))

(defn -main [& args]
(println input))