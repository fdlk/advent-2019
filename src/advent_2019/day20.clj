(ns advent-2019.day20
  (:require [advent-2019.core :refer [lines grid-neighbors A*]])
  (:require [clojure.string :refer [join]]))

(def maze (lines "day20.txt"))

(def maze-map
  (into {}
        (for [y (range (count maze))
              x (range (count (first maze)))
              :let [character (get-in maze [y x])]
              :when (not= character \space)]
          [[x y] character])))

(def portals
  (for [[[x y] character] (seq maze-map)
        :when (not (#{\# \.} character))
        entrance (grid-neighbors [x y])
        :when (= (maze-map entrance) \.)
        :let [direction (map - [x y] entrance)
              other-char-loc (map + [x y] direction)
              other-char (maze-map other-char-loc)]]
    [(join (sort [character other-char])) entrance]))

(def portals-grouped (group-by first portals))

(def mazey-coords (map first (filter (fn [x] (#{\. \#} (val x))) maze-map)))
(def mazey-xs (map first mazey-coords))
(def mazey-ys (map second mazey-coords))
(def outer-x #{(reduce min mazey-xs) (reduce max mazey-xs)})
(def outer-y #{(reduce min mazey-ys) (reduce max mazey-ys)})

(defn is-outer-ring
  [[x y]]
  (or (outer-x x) (outer-y y)))

(defn hop-through
  "hops through a portal"
  [location level]
  (let [portal-name (first (first (filter #(= location (second %)) portals)))
        candidates (portals-grouped portal-name)
        exit (first (filter #(not= location (second %)) candidates))
        new-level (if (is-outer-ring location) (dec level) (inc level))]
    [(second exit) new-level]))

(defn neighbors
  [part [location level]]
  (for [candidate (grid-neighbors location)
        :let [glyph (maze-map candidate)]
        :when (not= glyph \#)
        :let [result (if (= \. glyph)
                       [candidate level]
                       (hop-through location level))]
        :when (not= (first result) nil)
        :when (or (= part 1) (not (neg-int? (second result))))]
    result))

(defn heuristic [[_ level]] (* 30 level))

(def start (get-in portals-grouped ["AA" 0 1]))
(def finish (get-in portals-grouped ["ZZ" 0 1]))
(defn part1 [] (A* (partial neighbors 1) (constantly 0) [start 0] #{[finish 0]}))
(defn part2 [] (A* (partial neighbors 2) heuristic [start 0] #{[finish 0]}))

(defn -main [& _] (println "day20" (dec (count (part1))) (dec (count (part2)))))