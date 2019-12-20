(ns advent-2019.day20
  (:require [advent-2019.core :refer [lines manhattan grid-neighbors A*]])
  (:require [clojure.string :refer [join]]))

(def maze (lines "day20.txt"))

(def maze-map
  (into {}
        (for [y (range (count maze))
              x (range (count (first maze)))
              :let [character (get-in maze [y x])]
              :when (not= character \space )]
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

(defn hop-through
  "hops through a portal"
  [location]
  (let [portal-name (first (first (filter #(= location (second %)) portals)))
        candidates (portals-grouped portal-name)
        exit (first (filter #(not= location (second %)) candidates))]
    (second exit)))

(defn neighbors
  [location]
  (for [candidate (grid-neighbors location)
        :let [glyph (maze-map candidate)]
        :when (not= glyph \#)
        :let [result (if (= \. glyph)
                       candidate
                       (hop-through location))]
        :when (not= result nil)]
    result))

; (defn heuristic
;   [[x y keys-found]]
;   (->> keys-in-maze
;        (filter (fn [[_ _ character]] (not (keys-found character))))
;        (map (fn [[xkey ykey _]] (manhattan [x y] [xkey ykey])))
;        (reduce min 0)))

(def start (get-in portals-grouped ["AA" 0 1]))
(def finish (get-in portals-grouped ["ZZ" 0 1]))
(def part1 (A* neighbors (constantly 0) start #{finish}))

(defn -main [& _] (println "day20" (dec (count part1))))