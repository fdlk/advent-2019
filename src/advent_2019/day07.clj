(ns advent-2019.day07
  (:require [clojure.math.combinatorics :refer [permutations]])
  (:require [clojure.math.numeric-tower :refer [expt]]))

(def input [3,8,1001,8,10,8,105,1,0,0,21,42,67,84,97,118,199,280,361,442,99999,3,9,101,4,9,9,102,5,9,9,101,2,9,9,1002,9,2,9,4,9,99,3,9,101,5,9,9,102,5,9,9,1001,9,5,9,102,3,9,9,1001,9,2,9,4,9,99,3,9,1001,9,5,9,1002,9,2,9,1001,9,5,9,4,9,99,3,9,1001,9,5,9,1002,9,3,9,4,9,99,3,9,102,4,9,9,101,4,9,9,102,2,9,9,101,3,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99])

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
    [[program (+ ip 2)] value]))

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
  "Runs the program until it needs more input or halts"
  [p0, ip0, inputs0]
  (loop [state [p0 ip0] inputs inputs0 output nil]
    (let [[program ip] state]
      (case (opcode (nth program ip))
        99 [nil nil output]
        1 (recur (compute-and-set program ip) inputs output)
        2 (recur (compute-and-set program ip) inputs output)
        3 (if (empty? inputs)
            [program ip output]
            (recur (read-input program ip (first inputs)) (rest inputs) output))
        4 (let [[new-state output] (do-output program ip)]
            (recur new-state inputs output))
        5 (recur (jmp program ip) inputs output)
        6 (recur (jmp program ip) inputs output)
        7 (recur (compute-and-set program ip) inputs output)
        8 (recur (compute-and-set program ip) inputs output)))))

(defn run-loop
  [phase-settings]
  (loop [signal 0 states (map (fn [setting] [input 0 [setting]]) phase-settings)]
    (if (empty? states) signal
        (let [[program0 ip0 inputs] (first states)
              [program ip output] (run program0 ip0 (concat inputs [signal]))
              new-states (if (nil? program)
                           (rest states)
                           (concat (rest states) [[program ip []]]))]
          (recur output new-states)))))

(defn max-signal
  [phase-settings]
  (->> phase-settings
       (permutations)
       (map run-loop)
       (apply max)))

(def part1 (max-signal (range 5)))
(def part2 (max-signal (range 5 10)))
(defn -main [& args] (println part1 part2))