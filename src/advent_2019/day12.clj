(ns advent-2019.day12
  (:require [clojure.math.combinatorics :refer [combinations]]))

(def x0s [-4 -11 2 7])
(def y0s [3 -10 2 -1])
(def z0s [15 13 18 0])

; (def x0s [-8 5 2 9])
; (def y0s [-10 5 -7 -8])
; (def z0s [0 10 3 -3])

(defn updated-velocity
  "Updated velocity with gravity from other object"
  [position position-other velocity]
  (case (compare position position-other)
    0 velocity
    1 (dec velocity)
    -1 (inc velocity)))

(defn apply-gravity
  [positions velocities]
  (map-indexed
   (fn [index current-velocity]
     (reduce
      (fn [velocity other-index] (updated-velocity (positions index) (positions other-index) velocity))
      current-velocity
      (filter #(not= % index) (range 4))))
   velocities))

(defn step
  [[positions velocities]]
  (let [new-velocities (vec (apply-gravity positions velocities))
        new-positions (vec (map + positions new-velocities))]
   [new-positions new-velocities]))

(defn vec-energy
  [vec]
  (reduce + (map #(Math/abs %) vec)))

(defn state-energy
  [[positions velocities]]
  (* (vec-energy positions) (vec-energy velocities)))

(def part1
  (let [nsteps 1000
        [xs vxs] (nth (iterate step [x0s [0 0 0 0]]) nsteps)
        [ys vys] (nth (iterate step [y0s [0 0 0 0]]) nsteps)
        [zs vzs] (nth (iterate step [z0s [0 0 0 0]]) nsteps)]
    (reduce + (map state-energy (partition 2 (partition 3 (map #(Math/abs %) (interleave xs ys zs vxs vys vzs))))))
  ))

(defn -main [& args] (println "day12" part1))