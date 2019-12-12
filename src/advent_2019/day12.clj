(ns advent-2019.day12
  (:require [clojure.math.combinatorics :refer [combinations]])
  (:require [advent-2019.core :refer [lcm]]))

(def x0s [[-4 -11 2 7] [0 0 0 0]])
(def y0s [[3 -10 2 -1] [0 0 0 0]])
(def z0s [[15 13 18 0] [0 0 0 0]])

(defn apply-gravity
  [positions velocities]
  (map
   (fn [position velocity]
     (->> positions
          (map #(compare % position))
          (reduce +)
          (+ velocity)))
   positions velocities))

(defn step
  [[positions velocities]]
  (let [new-velocities (apply-gravity positions velocities)
        new-positions (map + positions new-velocities)]
    [new-positions new-velocities]))

(defn vec-energy
  [vec]
  (reduce + (map #(Math/abs %) vec)))

(defn state-energy
  [[positions velocities]]
  (* (vec-energy positions) (vec-energy velocities)))

(def part1
  (let [nsteps 1000
        [xs vxs] (nth (iterate step x0s) nsteps)
        [ys vys] (nth (iterate step y0s) nsteps)
        [zs vzs] (nth (iterate step z0s) nsteps)]
    (->> (interleave xs ys zs vxs vys vzs)
         (partition 3)
         (partition 2)
         (map state-energy)
         (reduce +))))

(defn period
  [state]
  (inc (.indexOf
        (->> state
             (iterate step)
             (rest))
        state)))

(def part2
  (->> [x0s y0s z0s]
       (map period)
       (reduce lcm)))

(defn -main [& args] (println "day12" part1 part2))