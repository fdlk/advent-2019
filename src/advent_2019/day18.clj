(ns advent-2019.day18
  (:require [advent-2019.core :refer [lines manhattan A*]])
  ; (:require [clojure.set :refer [difference]])
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def maze (lines "day18-2.txt"))

(defn grid-neighbors
  [p]
  (map (partial map + p) [[0 -1] [0 1] [-1 0] [1 0]]))

(def maze-map
  (into {}
        (for [y (range (count maze))
              x (range (count (first maze)))
              :let [character (get-in maze [y x])]]
          {[x y] character})))

(def quadrants [[(range 0 4) (range 0 4)]
                [(range 0 4) (range 4 8)]
                [(range 4 8) (range 0 4)]
                [(range 4 8) (range 4 8)]])

(defn keys-in-range
  [[xrange yrange]]
  (for [x xrange
        y yrange
        :let [character (maze-map [x y])]
        :when (Character/isLowerCase character)]
    [[x y] character]))

(def keys-per-quadrant
  (map keys-in-range quadrants))

(def keys-set-per-quadrant
  (vec (->> keys-per-quadrant
       (map (partial map second))
       (map (partial apply hash-set)))))

(defn has-key-left
  [quadrant-index keys-found]
  (some? (some #(not (keys-found %))
   (nth keys-set-per-quadrant quadrant-index))))

(defn neighbors
  "Gives the neigboring states. A state consists of the positions of all bots, the keys aquired and the index of the bot who has initiative"
  [[positions keys initiative]]
  (if (= initiative -1)
    (for [new-initiative (range 4)] [positions keys new-initiative])
    (for [neighbor (grid-neighbors (nth positions initiative))
          :let [[x y] neighbor
                map-char (maze-map [x y])
                key-found (Character/isLowerCase map-char)
                keys-found (if key-found (conj keys map-char) keys)
                new-positions (assoc positions initiative [x y])]
          :when (or (#{\. \@} map-char)
                    key-found
                    (keys (Character/toLowerCase map-char)))
          new-initiative (if key-found (range 4) [initiative])
          :when (or (not key-found) (has-key-left new-initiative keys-found))  
          ]
      [new-positions keys-found new-initiative])))

(def number-of-keys (count (keys-in-range [(range 8) (range 8)])))

(defn collected-all-keys
  [[_ found-keys _]]
  (= (count found-keys) number-of-keys))

(def memohattan (memoize #(manhattan % %2)))

(defn distance-to-closest-key
  [[positions keys-found _] quadrant-index]
  (->> (nth keys-per-quadrant quadrant-index)
       (filter (fn [state] (not (keys-found (second state)))))
       (map (fn [state] (memohattan (nth positions quadrant-index) (first state))))
       (#(if (empty? %) 0 (reduce min %)))))

(defn heuristic
  [[positions keys-found initiative]]
  (println (second keys-found))
  ; 0)
  ; (reduce + (map (partial distance-to-closest-key state) (range 4))))
  (if (= initiative -1) 0 (distance-to-closest-key [positions keys-found initiative] initiative)))

(def start-positions
  (vec (for [quadrant-index (range 4)
        :let  [[xrange yrange] (nth quadrants quadrant-index)]
        y yrange
        x xrange
        :let [character (maze-map [x y])]
        :when (= character \@)]
    [x y])))
  
(def start-node [start-positions #{} -1])

(defn part2 [] (A* neighbors heuristic start-node collected-all-keys))

(defn -main [& _] (println "day18" (part2)))