(ns advent-2019.day04
  (:require [clojure.string :refer [split join]])
  (:require [clojure.set :refer [intersection]])
  (:require [advent-2019.core :refer [lines parse-int manhattan]]))

(def input (range 402328 (inc 864247)))

(defn is-same [[d1 d2]] (= d1 d2))
(defn is-ascending [[d1 d2]] (<= (int d1) (int d2)))

(defn is-valid-part1
  [password]
  (let [pairs (partition 2 1 (str password))]
    (and 
      (some? (some is-same pairs))
      (every? is-ascending pairs))))

(def part1 (count (filter is-valid-part1 input)))

(defn -main
  [& args]
  (println part1))