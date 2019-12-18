(ns advent-2019.day18
  (:require [advent-2019.core :refer [lines]])
  (:require [clojure.data.priority-map :refer [priority-map]]))

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

; source: http://clj-me.cgrand.net/2010/09/04/a-in-clojure/
(defn A*
  "Finds a path between start and goal inside the graph described by edges
  (a map of edge to distance); estimate is an heuristic for the actual
  distance. Accepts a named option: :monotonic (default to true).
  Returns the path if found or nil."
  [edges estimate start goal & {mono :monotonic :or {mono true}}]
  (let [f (memoize #(estimate % goal)) ; unsure the memoization is worthy
        neighbours (reduce (fn [m [a b]] (assoc m a (conj (m a #{}) b)))
                           {} (keys edges))]
    (loop [q (priority-map start (f start))
           preds {}
           shortest {start 0}
           done #{}]
      (when-let [[x hx] (peek q)]
        (if (= goal x)
          (reverse (take-while identity (iterate preds goal)))
          (let [dx (- hx (f x))
                bn (for [n (remove done (neighbours x))
                         :let [hn (+ dx (edges [x n]) (f n))
                               sn (shortest n Double/POSITIVE_INFINITY)]
                         :when (< hn sn)]
                     [n hn])]
            (recur (into (pop q) bn)
                   (into preds (for [[n] bn] [n x]))
                   (into shortest bn)
                   (if mono (conj done x) done))))))))

(defn -main [& _] (println "day18"))