(ns advent-2019.core
  (:require [clojure.java.io :refer [resource]])
  (:require [clojure.string :refer [split-lines]])
  (:require [clojure.data.priority-map :refer [priority-map]])
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

(defn grid-neighbors
  [p]
  (map (partial map + p) [[0 -1] [0 1] [-1 0] [1 0]]))

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

; ; source: http://clj-me.cgrand.net/2010/09/04/a-in-clojure/
; ; adapted for this problem
(defn A*
  "Finds a path between start and goal inside the graph described by a neighbor
  function that returns cost and neighbors; estimate is an heuristic for the actual
  distance.
  Returns the cost and the path if found or nil."
  [neighbor-fn estimate start are-we-done]
  (let [f (memoize #(estimate %))] ; unsure the memoization is worthy
    (loop [q (priority-map start (f start))
           preds {}
           shortest {start 0}
           done #{}]
      (when-let [[x hx] (peek q)]
        (if (are-we-done x)
          [hx (reverse (take-while identity (iterate preds x)))]
          (let [dx (- hx (f x))
                bn (for [[cost n] (filter (complement done) (neighbor-fn x))
                         :let [hn (+ dx cost (f n))
                               sn (shortest n Double/POSITIVE_INFINITY)]
                         :when (< hn sn)]
                     [n hn])]
            (recur (into (pop q) bn)
                   (into preds (for [[n] bn] [n x]))
                   (into shortest bn)
                   (conj done x))))))))