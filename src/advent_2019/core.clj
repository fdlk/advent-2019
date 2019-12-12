(ns advent-2019.core
  (:require [clojure.java.io :refer [resource]])
  (:require [clojure.string :refer [split-lines]])
  (:gen-class))

(defn parse-int
  [number-string]
  "parses string to integer"
  (Integer/parseInt number-string 10))

(defn sum
  [sequence]
  "computes the sum of the elements in the sequence"
  (reduce + sequence))

(defn manhattan
  "Determines the manhattan distance between two points"
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn lines
  [name]
  "reads lines from a resource"
  (->> name
       (resource)
       (slurp)
       (split-lines)))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))