(ns advent-2019.day14
  (:require [advent-2019.core :refer [lines parse-int]])
  (:require [clojure.string :refer [split trim]])
  (:require [clojure.math.numeric-tower :refer [ceil]])
  (:require [clojure.set :refer [union]]))

(defn parse-ingredient
  "Parses name count tuple"
  [ingredient]
  (let [[count name] (split ingredient #" ")]
    {:count (parse-int count) :name (trim name)}))

(defn parse-reaction
  "Parses reaction line"
  [line]
  (let [[left right] (split line #" => ")
        ingredients (split left #", ")]
    {:ingredients (map parse-ingredient ingredients) :product (parse-ingredient right)}))

(def reactions
  (->> "day14.txt"
       (lines)
       (map parse-reaction)))

(def reaction-map
  (reduce
   (fn [lookup reaction]
     (assoc lookup
            (get-in reaction [:product :name])
            reaction))
   {}
   reactions))

(defn multiply-count
  [times tuple]
  (assoc tuple :count (* times (:count tuple))))

(defn multiply-reaction
  [times reaction]
  {:ingredients (map (partial multiply-count times) (:ingredients reaction))
   :product (multiply-count times (:product reaction))})

(defn what-do-i-need
  "Determines what's needed to produce a desired amount of product"
  [tuple]
  (let [desired (:count tuple)
        product (:name tuple)
        reaction (reaction-map product)
        produced-per-execution (get-in reaction [:product :count])
        number-of-times (bigint (ceil (/ desired produced-per-execution)))]
    (:ingredients (multiply-reaction number-of-times reaction))))

(defn combine-ingredients
  [i0 i1]
  (let [grouped (group-by :name (concat i0 i1))]
    (map #(hash-map :name (:name (first %))
                    :count (reduce + (map :count %))) (vals grouped))))

(defn needed-for-products-internal
  [products]
  (apply union (map (fn [product] (set (map :name (:ingredients (reaction-map product)))) ) products)))

(defn needed-for-products
  [products]
  (loop [ingredients (needed-for-products-internal products)]
      (let [updated (union ingredients (needed-for-products-internal ingredients))]
        (if (= updated ingredients) ingredients (recur updated)))))

(defn what-next
  [needed]
  (let [products (map :name needed)
        ingredients (needed-for-products products)
        not-needed (filter (fn [candidate] (not-any? #(= (:name candidate) %) ingredients)) needed)]
    (filter #(not= "ORE" (:name %)) not-needed)))

(defn how-much-ore-for
  [count]
  (loop [needed (what-do-i-need {:count count, :name "FUEL"})]
    (let [ingredient-list (what-next needed)]
      (if (empty? ingredient-list) (:count (first needed))
          (let [ingredient (first ingredient-list)
                the-rest (filter (partial not= ingredient) needed)
                needed-for-ingredient (what-do-i-need ingredient)
                combined (combine-ingredients needed-for-ingredient the-rest)]
            (recur combined))))))

(def part1 (how-much-ore-for 1))

(defn bisect
  [f a0 b0]
  (loop [a a0 b b0]
  (let [mid (quot (+ a b) 2)
        comparison (f mid)]
    (if (= mid a) a
        (case comparison
          0 mid
          1 (recur a mid)
          -1 (recur mid b))))))

(def trillion 1000000000000N)

(def part2 (bisect #(compare (how-much-ore-for %) trillion) (quot trillion part1) trillion))

(defn -main [& _] (println "day14" part1 part2))