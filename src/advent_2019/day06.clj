(ns advent-2019.day06
  (:require [advent-2019.core :refer [sum lines]])
  (:require [clojure.string :refer [split]]))

(def input
  (->> "day06.txt"
       (lines)
       (map #(split % #"\)"))
       (reduce (fn [coll [v k]] (assoc coll k v)) {})))

(defn -main [& args]
  (println (input "ZL4")))