(ns advent-2019.day24
  (:require [advent-2019.core :refer [lines grid-neighbors sum]])
  (:require [clojure.string :refer [join]]))

(def input (lines "day24.txt"))

(def input-map (into {}
                     (for [y (range 5)
                           x (range 5)]
                       {[x y] (if (#{\#} (get (get input y) x)) 1 0)})))

(defn next-value
  [neighbor-fn state pos]
  (let [current (get state pos)
        active-neighbors (sum (for [neighbor (neighbor-fn pos)]
                                (get state neighbor 0)))]
    (if (= current 1)
      (if (= active-neighbors 1) 1 0)
      (if (#{1 2} active-neighbors) 1 0))))

(defn next-state
  [neighbor-fn state]
  (into {}
        (map
         (fn [pos] {pos (next-value neighbor-fn state pos)})
         (keys state))))

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
  (map biodiversity-value (iterate (partial next-state grid-neighbors) input-map)))

(defn first-duplicate
  [coll]
  (reduce (fn [acc elt]
            (if (acc elt)
              (reduced elt)
              (conj acc elt)))
          #{}
          coll))

(def part1 (first-duplicate biodiversities))

(def lower-level-neighbors
  {[2 1] [[0 0] [1 0] [2 0] [3 0] [4 0]]
   [3 2] [[4 0] [4 1] [4 2] [4 3] [4 4]]
   [2 3] [[0 4] [1 4] [2 4] [3 4] [4 4]]
   [1 2] [[0 0] [0 1] [0 2] [0 3] [0 4]]})

(def higher-level-neighbors
  {[0 0] [[2 1] [1 2]],
   [1 0] [[2 1]],
   [2 0] [[2 1]],
   [3 0] [[2 1]],
   [4 0] [[2 1] [3 2]],
   [4 1] [[3 2]],
   [4 2] [[3 2]],
   [4 3] [[3 2]],
   [4 4] [[3 2] [2 3]],
   [0 4] [[2 3] [1 2]],
   [1 4] [[2 3]],
   [2 4] [[2 3]],
   [3 4] [[2 3]],
   [0 1] [[1 2]],
   [0 2] [[1 2]],
   [0 3] [[1 2]]})

(defn fractal-neighbors
  [[pos level]]
  (let [neighbors-on-this-level (for [[x y] (grid-neighbors pos)
                                      :when (not= [x y] [2 2])
                                      :when (<= 0 x 4)
                                      :when (<= 0 y 4)]
                                  [[x y] level])
        neighbors-on-lower-level (for [neighbor (lower-level-neighbors pos)] [neighbor (dec level)])
        neighbors-on-higher-level (for [neighbor (higher-level-neighbors pos)] [neighbor (inc level)])]
    (concat neighbors-on-this-level neighbors-on-higher-level neighbors-on-lower-level)))

(def initial-state (into {}
                         (for [[pos value] (seq input-map)
                               level (range -200 201)
                               :when (not= pos [2 2])]
                           (if (= level 0) {[pos 0] value} {[pos level] 0}))))

(defn number-of-bugs [state] (sum (vals state)))

(def part2 (->> initial-state 
                (iterate (partial next-state fractal-neighbors))
                (drop 200)
                (first)
                (number-of-bugs)))

(defn -main [& _] (println "day24" part1 part2))