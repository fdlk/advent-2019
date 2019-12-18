(ns advent-2019.day18
  (:require [advent-2019.core :refer [lines parse-int]])
  (:require [clojure.string :refer [lower-case]]))

(def maze (lines "day18.txt"))

(defn grid-neighbors
  [p]
  (map (partial map + p) [[0 -1] [0 1] [-1 0] [1 0]]))

(defn neighbors
  [[x0 y0 keys]]
  (for [neighbor (grid-neighbors [x0 y0])
        :let [[x y] neighbor
              map-char (get-in maze [y x])
              key-found (Character/isLowerCase map-char)]
        :when (or (#{\. \@} map-char)
                  key-found
                  (keys (Character/toLowerCase map-char)))]
    [x y (if key-found (conj keys map-char) keys)]))

(defn -main [& _] (println "day18"))