(ns advent-2019.day04)

(def input (map str (range 402328 (inc 864247))))

(defn is-same [[d1 d2]] (= d1 d2))
(def is-not-same (complement is-same))
(defn is-ascending [[d1 d2]] (<= (int d1) (int d2)))
(defn is-same-middle [[d1 d2 d3 d4]] (and (is-same [d2 d3]) (is-not-same [d1 d2]) (is-not-same [d3 d4])))

(defn is-valid-part1 [password]
  (let [pairs (partition 2 1 password)]
    (and
     (some? (some is-same pairs))
     (every? is-ascending pairs))))

(defn is-also-valid-part2 [password]
  (let [quads (partition 4 1 (concat "x" password "x"))]
    (some? (some is-same-middle quads))))

(def part1 (filter is-valid-part1 input))
(def part2 (filter is-also-valid-part2 part1))

(defn -main [& args]
  (println (count part1) (count part2)))