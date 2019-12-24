(ns advent-2019.day24
  (:require [advent-2019.core :refer [lines grid-neighbors sum]])
  (:require [clojure.string :refer [join]]))

(def input (lines "day24.txt"))

(def input-map (into {}
                     (for [y (range 5)
                           x (range 5)]
                       {[x y] (if (#{\#} (get (get input y) x)) 1 0)})))

(defn next-value
  [tile pos]
  (let [current (get tile pos)
        active-neighbors (sum (for [neighbor (grid-neighbors pos)]
                                (get tile neighbor 0)))]
    (if (= current 1)
      (if (= active-neighbors 1) 1 0)
      (if (#{1 2} active-neighbors) 1 0))))

(defn next-tile
  [tile]
  (into {}
        (map
         (fn [pos] {pos (next-value tile pos)})
         (keys tile))))

(defn printable-tile
  [tile]
  (->> (for [y (range 5)
             x (range 5)] [x y])
       (map tile)
       (map #(case % 1 \# 0 \.))
       (partition 5)
       (map join)
       (join \newline)))

(defn biodiversity-value
  [tile]
  (reduce
   (fn [sofar [[x y] bit]]
     (if (= bit 1)
       (bit-set sofar (+ x (* y 5)))
       sofar))
   0
   tile))

(def biodiversities
  (map biodiversity-value (iterate next-tile input-map)))

(defn first-duplicate
  [coll]
  (reduce (fn [acc elt]
            (if (acc elt)
              (reduced elt)
              (conj acc elt)))
          #{}
          coll))

(def part1 (first-duplicate biodiversities))

(defn -main [& _] (println "day24" part1))