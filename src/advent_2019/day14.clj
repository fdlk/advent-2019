(ns advent-2019.day14
  (:require [advent-2019.core :refer [lines]])
  (:require [clojure.string :refer [split]]))

(defn parse-ingredient
  [ingredient]
  (let [[count name] (split ingredient #" ")]
    {:count count :name name}))

(defn parse-reaction
  [line]
  "Parses reaction line"
  (let [[left right] (split line #" => ")
        ingredients (split left #", ")]
    {:ingredients (map parse-ingredient ingredients) :product (parse-ingredient right)}))

(def input
  (->> "day14.txt"
       (lines)
       (map parse-reaction)))

(defn -main [& _] (println "day14" input))