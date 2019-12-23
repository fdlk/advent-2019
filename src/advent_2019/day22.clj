(ns advent-2019.day22
  (:require [advent-2019.core :refer [lines parse-int]]))

(def deck-size 10007)

(defn parse
  [line]
  (let [cut-match (re-matches #"cut (-?\d+)" line)
        deal-match (re-matches #"deal with increment (\d+)" line)]
    (if (= line "deal into new stack")
      [:deal-stack]
      (if (some? cut-match)
        [:cut (parse-int (second cut-match))]
        (if (some? deal-match)
          [:deal-increment (parse-int (second deal-match))]
          nil)))))

(defn deal-stack
  [index]
  (- (- deck-size 1) index))

(defn cut
  [amount index]
  (mod (+ deck-size index (- amount))
       deck-size))

(defn deal-with-increment
  [increment index]
  (mod (* increment index) deck-size))

(defn shuffle-step
  [index technique]
  (case (first technique)
    :deal-stack (deal-stack index)
    :cut (cut (second technique) index)
    :deal-increment (deal-with-increment (second technique) index)))

(defn shuffle-deck
  [index techniques]
  (reduce shuffle-step index techniques))

(def input (->> "day22.txt"
                (lines)
                (map parse)))

(def part1 (shuffle-deck 2019 input))

(defn -main [& _] (println "Day 22" part1))