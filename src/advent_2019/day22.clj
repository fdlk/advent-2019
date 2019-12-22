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
  [deck] 
  (reverse deck))

(defn cut 
  [amount deck]
  (if (>= amount 0)
    (concat (drop amount deck) (take amount deck))
    (cut (+ deck-size amount) deck)))

(defn deal-with-increment
  [increment deck]
  (let [quotient (quot deck-size increment)
        remainder (rem deck-size increment)
        leftover (- increment remainder)]
    (loop [to-deal deck
           stacks (vec (repeat increment nil))
           index 0]
      (if (empty? to-deal)
        (take deck-size (apply interleave stacks))
        (let [deal-one-extra (< index remainder)
              amount-to-deal (if deal-one-extra (inc quotient) quotient)
              stack (take amount-to-deal to-deal)]
          (recur (drop amount-to-deal to-deal) 
                 (assoc stacks index (if (not deal-one-extra) (concat stack [nil]) stack))
                 (rem (+ leftover index) increment)))))))

(defn shuffle-step
  [deck technique]
  (case (first technique)
    :deal-stack (deal-stack deck)
    :cut (cut (second technique) deck)
    :deal-increment (deal-with-increment (second technique) deck)))

(defn shuffle-deck
  [deck techniques]
  (reduce shuffle-step deck techniques))

(def input (->> "day22.txt"
                (lines)
                (map parse)))

(def part1 (.indexOf (shuffle-deck (range deck-size) input) 2019))

(defn -main [& _] (println "Day 22" part1))