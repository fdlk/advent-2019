(ns advent-2019.day22
  (:require [advent-2019.intcode :refer [run]])
  (:require [clojure.string :refer [starts-with?]])
  (:require [advent-2019.core :refer [lines parse-int]]))

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

(def input (->> "day22.txt"
                (lines)
                (map parse)))

(defn -main [& _] (println "Day 22" input))