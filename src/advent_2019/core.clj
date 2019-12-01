(ns advent-2019.core
  (:gen-class))

(defn parse-int
  [number-string]
  "parses string to integer"
  (Integer/parseInt number-string 10))