(ns advent-2019.day07
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.math.numeric-tower :refer [expt]]))

(def input
  [3,8,1001,8,10,8,105,1,0,0,21,42,67,84,97,118,199,280,361,442,99999,3,9,101,4,9,9,102,5,9,9,101,2,9,9,1002,9,2,9,4,9,99,3,9,101,5,9,9,102,5,9,9,1001,9,5,9,102,3,9,9,1001,9,2,9,4,9,99,3,9,1001,9,5,9,1002,9,2,9,1001,9,5,9,4,9,99,3,9,1001,9,5,9,1002,9,3,9,4,9,99,3,9,102,4,9,9,101,4,9,9,102,2,9,9,101,3,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99])

(defn opcode [instruction] (mod instruction 100))
(defn parameter-mode [instruction index] (rem (quot instruction (expt 10 (+ index 2))) 10))
(defn operand-value [operand mode program] (if (= mode 0) (program operand) operand))

(defn compute-and-set
  [program, ip]
  (let [[instruction op1 op2 target] (subvec program ip)
        value1 (operand-value op1 (parameter-mode instruction 0) program)
        value2 (operand-value op2 (parameter-mode instruction 1) program)
        value-to-set (case (opcode instruction)
                       1 (+ value1 value2)
                       2 (* value1 value2)
                       7 (if (< value1 value2) 1 0)
                       8 (if (= value1 value2) 1 0))]
    [(assoc program target value-to-set) (+ ip 4)]))

(defn read-input
  [program, ip, value]
  (let [[_ target] (subvec program ip)]
    [(assoc program target value) (+ ip 2)]))

(defn do-output
  [program, ip]
  (let [[instruction op] (subvec program ip)
        value (operand-value op (parameter-mode instruction 0) program)]
    value))

(defn jmp
  [program, ip]
  (let [[instruction op1 op2] (subvec program ip)
        value1 (operand-value op1 (parameter-mode instruction 0) program)
        value2 (operand-value op2 (parameter-mode instruction 1) program)
        do-jump (case (opcode instruction)
                  5 (not= value1 0)
                  6 (= value1 0))]
    [program (if do-jump value2 (+ ip 3))]))

(defn run
  "Runs the program to completion"
  [p0, ip0, inputs0]
  (loop [state [p0 ip0] inputs inputs0]
    (let [[program ip] state]
      (case (opcode (program ip))
        1 (recur (compute-and-set program ip) inputs)
        2 (recur (compute-and-set program ip) inputs)
        3 (recur (read-input program ip (first inputs)) (rest inputs))
        4 (do-output program ip)
        5 (recur (jmp program ip) inputs)
        6 (recur (jmp program ip) inputs)
        7 (recur (compute-and-set program ip) inputs)
        8 (recur (compute-and-set program ip) inputs)))))

(defn run-series
  [phase-settings]
  (reduce
   (fn [signal phase-setting] (run input 0 [phase-setting signal]))
   0
   phase-settings))

(def part1
  (apply max (->> (combo/permutations (range 5))
                  (map run-series))))

(defn -main [& args]
  (println part1))