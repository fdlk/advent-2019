(ns advent-2019.day18
  (:require [advent-2019.core :refer [lines manhattan]])
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
; adapted for this problem
(defn A*
  "Finds a path between start and goal inside the graph described by edges
  (a map of edge to distance); estimate is an heuristic for the actual
  distance. Accepts a named option: :monotonic (default to true).
  Returns the path if found or nil."
  [neighbor-fn estimate start are-we-done & {mono :monotonic :or {mono true}}]
  (let [f (memoize #(estimate %))] ; unsure the memoization is worthy
    (loop [q (priority-map start (f start))
           preds {}
           shortest {start 0}
           done #{}]
      (when-let [[x hx] (peek q)]
        (if (are-we-done x)
          (reverse (take-while identity (iterate preds x)))
          (let [dx (- hx (f x))
                bn (for [n (remove done (neighbor-fn x))
                         :let [hn (+ dx 1 (f n))
                               sn (shortest n Double/POSITIVE_INFINITY)]
                         :when (< hn sn)]
                     [n hn])]
            (recur (into (pop q) bn)
                   (into preds (for [[n] bn] [n x]))
                   (into shortest bn)
                   (if mono (conj done x) done))))))))

(def keys-in-maze
  (for [y (range (count maze))
        x (range (count (first maze)))
        :let [character (get-in maze [y x])]
        :when (Character/isLowerCase character)]
    [x y character]))

(defn collected-all-keys
  [[_ _ found-keys]]
  (>=(count found-keys) (count keys-in-maze)))

(defn heuristic
  [[x y keys-found]]
  (->> keys-in-maze
       (filter (fn [[_ _ character]] (not (keys-found character))))
       (map (fn [[xkey ykey _]] (manhattan [x y] [xkey ykey])))
       (reduce min 0)))

(def start-node
  (first (for [y (range (count maze))
        x (range (count (first maze)))
        :let [character (get-in maze [y x])]
        :when (= character \@)]
    [x y #{}])))

(defn part1 [] (A* neighbors heuristic start-node collected-all-keys))

(defn -main [& _] (println "day18" (dec (count (part1)))))