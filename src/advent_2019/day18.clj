(ns advent-2019.day18
  (:require [advent-2019.core :refer [lines manhattan A* grid-neighbors]])
  (:require [clojure.set :refer [map-invert difference union]])
  (:require [clojure.math.numeric-tower :refer [ceil]]))

(def maze (lines "day18-2.txt"))

(def maze-map
  (into {}
        (for [y (range (count maze))
              x (range (count (first maze)))
              :let [character (get-in maze [y x])]]
          {[x y] character})))

(def lookup-maze (map-invert maze-map))

(def maze-size (count maze))
(def quadrant-size (int (ceil (/ maze-size 2))))
(def lower-range (range 0 quadrant-size))
(def upper-range (range (dec quadrant-size) maze-size))

(def quadrants [[lower-range lower-range]
                [lower-range upper-range]
                [upper-range lower-range]
                [upper-range upper-range]])

(def start-positions
  (vec (for [quadrant-index (range 4)
             :let  [[xrange yrange] (nth quadrants quadrant-index)]
             y yrange
             x xrange
             :let [character (maze-map [x y])]
             :when (= character \@)]
         [x y])))

(defn is-key [glyph] (Character/isLowerCase glyph))
(defn is-door [glyph] (Character/isUpperCase glyph))
(defn is-wall [glyph] (= \# glyph))
(defn is-accessible
  [glyph keys-in-pocket]
  (if (is-door glyph)
    (keys-in-pocket (Character/toLowerCase glyph))
    (not (is-wall glyph))))

(defn maze-neighbors
  [keys-in-pocket position]
  (let [pos-glyph (maze-map position)]
    (if (and (is-key pos-glyph) (not (keys-in-pocket pos-glyph)))
      [] ; We're standing on a newly found key, don't run further
      (for [neighbor (grid-neighbors position)
            :let [glyph (maze-map neighbor)]
            :when (is-accessible glyph keys-in-pocket)]
        [1 neighbor]))))

(defn distance-to-closest-key
  [key-locations position]
  (reduce min (map (partial manhattan position) key-locations)))

(defn new-key-found
  [left-to-find position]
  (let [glyph (maze-map position)
        result (and (is-key glyph) (some? (left-to-find glyph)))]
    result))

(defn find-all-keys
  [position keys-in-pocket keys-to-find]
  (loop [found []
         left-to-find keys-to-find]
    (if (empty? left-to-find) found
        (let [key-locations (map lookup-maze left-to-find)
              solution (A*
                        (partial maze-neighbors keys-in-pocket)
                        (partial distance-to-closest-key key-locations)
                        position
                        (partial new-key-found left-to-find))]
          (if (nil? solution)
            found
            (let [[cost path] solution
                  location (last path)
                  glyph (maze-map location)
                  new-found (conj found [cost glyph location])
                  new-to-find (disj left-to-find glyph)]
              (recur new-found new-to-find)))))))

(defn keys-in-quadrant
  [[xrange yrange]]
  (apply hash-set (for [x xrange
                        y yrange
                        :let [glyph (maze-map [x y])]
                        :when (is-key glyph)]
                    glyph)))

(def keys-per-quadrant (map keys-in-quadrant quadrants))

(defn key-neighbors
  [[positions keys-in-pocketses]]
  (for [quadrant (range 4)
        :let [position (nth positions quadrant)
              keys-in-pocket (nth keys-in-pocketses quadrant)
              keys-in-this-quadrant (nth keys-per-quadrant quadrant)
              keys-to-find (difference keys-in-this-quadrant keys-in-pocket)]
        :when (seq keys-to-find)
        found (find-all-keys position (apply union keys-in-pocketses) keys-to-find)]
    (let [[cost glyph position] found]
      [cost [(assoc positions quadrant position)
             (assoc keys-in-pocketses quadrant (conj keys-in-pocket glyph))]])))

(def number-of-keys (count (filter is-key (keys lookup-maze))))

(defn part2 [] (first (A* key-neighbors
               (constantly 0)
               [start-positions [#{} #{} #{} #{}]]
               #(= number-of-keys (reduce + (map count (second %)))))))

(defn -main [& _] (println "day18" (part2)))