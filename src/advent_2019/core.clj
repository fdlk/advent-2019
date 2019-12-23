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

(defn abs
  [x]
  (if (< x 0) (- x) x))


(defn extended-gcd
  "The extended Euclidean algorithm--using Clojure code from RosettaCode for Extended Eucliean
  (see http://en.wikipedia.orwiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs with the result: gcd followed by bezout coefficients "
  [a b]
  (cond (zero? a) [(abs b) 0 1]
        (zero? b) [(abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (abs b)
                     r0 (abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn mul-inv
  " Get inverse using extended gcd.  Extended GCD returns
    gcd followed by bezout coefficients. We want the 1st coefficients
   (i.e. second of extend-gcd result).  We compute mod base so result
    is between 0..(base-1) "
  [a b]
  (let [b (if (neg? b) (- b) b)
        a (if (neg? a) (- b (mod (- a) b)) a)
        egcd (extended-gcd a b)]
    (if (= (first egcd) 1)
      (mod (second egcd) b)
      (str "No inverse since gcd is: " (first egcd)))))

; source: https://rosettacode.org/wiki/Modular_exponentiation#Clojure
(defn mod-pow
  " b^e mod m (using Java which solves some cases the pure clojure method has to be modified to tackle--i.e. with large b & e and 
    calculation simplications when gcd(b, m) == 1 and gcd(e, m) == 1) "
  [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

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