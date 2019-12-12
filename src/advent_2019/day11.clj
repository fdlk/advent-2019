(ns advent-2019.day11
  (:require [clojure.string :refer [join]])
  (:require [clojure.math.numeric-tower :refer [expt]]))

(def input [3,8,1005,8,314,1106,0,11,0,0,0,104,1,104,0,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,28,2,2,16,10,1,1108,7,10,1006,0,10,1,5,14,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,65,1006,0,59,2,109,1,10,1006,0,51,2,1003,12,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,101,1006,0,34,1,1106,0,10,1,1101,17,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,1001,8,0,135,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,156,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,178,1,108,19,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,204,1,1006,17,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,230,1006,0,67,1,103,11,10,1,1009,19,10,1,109,10,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,101,0,8,268,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1002,8,1,290,2,108,13,10,101,1,9,9,1007,9,989,10,1005,10,15,99,109,636,104,0,104,1,21101,48210224024,0,1,21101,0,331,0,1105,1,435,21101,0,937264165644,1,21101,0,342,0,1105,1,435,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,235354025051,0,1,21101,389,0,0,1105,1,435,21102,29166169280,1,1,21102,400,1,0,1105,1,435,3,10,104,0,104,0,3,10,104,0,104,0,21102,709475849060,1,1,21102,1,423,0,1106,0,435,21102,868498428684,1,1,21101,434,0,0,1105,1,435,99,109,2,21201,-1,0,1,21101,0,40,2,21102,1,466,3,21101,456,0,0,1105,1,499,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,461,462,477,4,0,1001,461,1,461,108,4,461,10,1006,10,493,1101,0,0,461,109,-2,2106,0,0,0,109,4,2102,1,-1,498,1207,-3,0,10,1006,10,516,21102,1,0,-3,21201,-3,0,1,21201,-2,0,2,21102,1,1,3,21102,535,1,0,1106,0,540,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,563,2207,-4,-2,10,1006,10,563,21202,-4,1,-4,1106,0,631,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21101,582,0,0,1105,1,540,22102,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,601,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,623,22102,1,-1,1,21101,623,0,0,105,1,498,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0])

(def inputmap
  (->> input
       (map-indexed (fn [index instruction] [index instruction]))
       (reduce (fn [coll [k v]] (assoc coll k v)) {})))

(defn opcode [instruction] (mod instruction 100))
(defn parameter-mode [instruction index] (rem (quot instruction (expt 10 (+ index 2))) 10))
(defn target-value [target mode program]
  (if (not= mode 2) target (+ target (program :rb))))
(defn operand-value [operand mode program]
  (let [value (case mode
                0 (program operand)
                1 operand
                2 (program (+ operand (program :rb))))]
    (or value 0)))

(defn compute-and-set
  [program, ip]
  (let [instruction (program ip)
        op1 (program (+ ip 1))
        op2 (program (+ ip 2))
        target (program (+ ip 3))
        value1 (operand-value op1 (parameter-mode instruction 0) program)
        value2 (operand-value op2 (parameter-mode instruction 1) program)
        target-to-set (target-value target (parameter-mode instruction 2) program)
        value-to-set (case (opcode instruction)
                       1 (+ value1 value2)
                       2 (* value1 value2)
                       7 (if (< value1 value2) 1 0)
                       8 (if (= value1 value2) 1 0))]
    [(assoc program target-to-set value-to-set) (+ ip 4)]))

(defn adjust-relative-base
  [program, ip]
  (let [instruction (program ip)
        op (program (+ ip 1))
        value (operand-value op (parameter-mode instruction 0) program)]
    [(assoc program :rb (+ value (program :rb))) (+ ip 2)]))

(defn read-input
  [program, ip, value]
  (let [instruction (program ip)
        target (program (+ ip 1))
        target-to-set (target-value target (parameter-mode instruction 0) program)]
    [(assoc program target-to-set value) (+ ip 2)]))

(defn do-output
  [program, ip]
  (let [instruction (program ip)
        op (program (+ ip 1))
        value (operand-value op (parameter-mode instruction 0) program)]
    [[program (+ ip 2)] value]))

(defn jmp
  [program, ip]
  (let [instruction (program ip)
        op1 (program (+ ip 1))
        op2 (program (+ ip 2))
        value1 (operand-value op1 (parameter-mode instruction 0) program)
        value2 (operand-value op2 (parameter-mode instruction 1) program)
        do-jump (case (opcode instruction)
                  5 (not= value1 0)
                  6 (= value1 0))]
    [program (if do-jump value2 (+ ip 3))]))

(defn run
  "Runs the program until it needs more input or halts"
  [state0, input0]
  (loop [state state0 input input0 output []]
    (let [[program ip] state]
      (case (opcode (program ip))
        99 [nil output]
        1 (recur (compute-and-set program ip) input output)
        2 (recur (compute-and-set program ip) input output)
        3 (if (nil? input) [[program ip] output]
              (recur (read-input program ip input) nil output))
        4 (let [[new-state new-output] (do-output program ip)]
            (recur new-state input (conj output new-output)))
        5 (recur (jmp program ip) input output)
        6 (recur (jmp program ip) input output)
        7 (recur (compute-and-set program ip) input output)
        8 (recur (compute-and-set program ip) input output)
        9 (recur (adjust-relative-base program ip) input output)))))

(defn turn
  [facing instruction]
  (case facing
    :up    (if (= instruction 0) :left :right)
    :left  (if (= instruction 0) :down :up)
    :down  (if (= instruction 0) :right :left)
    :right (if (= instruction 0) :up :down)))

(defn move
  [[x y] facing]
  (case facing
    :up    [x (- y 1)]
    :left  [(- x 1) y]
    :down  [x (+ y 1)]
    :right [(+ x 1) y]))

(defn robot
  [color]
  (loop [cpu [(assoc inputmap :rb 0) 0] painted {[0 0] color} facing :up location [0 0]]
    (let [color (or (painted location) 0)
          [new-cpu [new-color instruction]] (run cpu color)
          new-painted (assoc painted location new-color)
          new-facing (turn facing instruction)
          new-location (move location new-facing)]
      (if (nil? new-cpu) new-painted
          (recur new-cpu new-painted new-facing new-location)))))

(defn draw
  [painted]
  (let [xs (map first (keys painted))
        ys (map second (keys painted))
        xmin (apply min xs)
        xmax (inc (apply max xs))
        ymin (apply min ys)
        ymax (inc (apply max ys))]
    (->> (for [y (range ymin ymax)
               x (range xmin xmax)]
           (if (= (painted [x y]) 1) \█ \ ))
         (partition (count (range xmin xmax)))
         (map join)
         (join \newline))))

(def part1 (count (robot 0)))
(def part2 (draw (robot 1)))

(defn -main [& args] (println part1) (println part2))