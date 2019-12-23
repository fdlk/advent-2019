(ns advent-2019.day22
  (:require [advent-2019.core :refer [lines parse-int mul-inv mod-pow]]))

(def deck-size 119315717514047N)

(defn parse
  [line]
  (let [cut-match (re-matches #"cut (-?\d+)" line)
        deal-match (re-matches #"deal with increment (\d+)" line)]
    (if (= line "deal into new stack")
      [:deal-stack]
      (if (some? cut-match)
        [:cut (parse-int (second cut-match))]
        (if (some? deal-match)
          [:deal-increment (parse-int (second deal-match)) (mul-inv (parse-int (second deal-match)) deck-size)]
          nil)))))

(defn deal-stack
  [index]
  (- deck-size (inc index)))

(defn cut
  [amount index]
  (mod (+ deck-size index amount) deck-size))

(defn deal-with-increment
  [increment-inverse index]
  (mod (* increment-inverse index) deck-size))

(defn shuffle-step
  [index technique]
  (case (first technique)
    :deal-stack (deal-stack index)
    :cut (cut (second technique) index)
    :deal-increment (deal-with-increment (nth technique 2) index)))

(defn shuffle-deck
  [index techniques]
  (reduce shuffle-step index techniques))

(def input (->> "day22.txt"
                (lines)
                (map parse)))

; Determine m and b for which the shuffle f(x) = mx + b mod deck-size
(def b (shuffle-deck 0 (reverse input)))
(def m (- (shuffle-deck 1 (reverse input)) b))

(defn nth-iteration
  "Determines the nth iteration of linear function f(x) = mx + b starting with x=x0
   See https://demonstrations.wolfram.com/IteratingLinearFunctions/img/desc2292922151188717492.png"
  [b m n x0]
  (let [inv-one-minus-m (mul-inv (- 1 m) deck-size)]
    (mod (+ (* inv-one-minus-m b)
            (* (mod-pow m n deck-size) (- x0 (* b inv-one-minus-m)))) deck-size)))

(defn -main [& _] (println "Day 22" (nth-iteration b m 101741582076661N 2020)))