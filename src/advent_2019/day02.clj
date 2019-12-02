(ns advent-2019.day02)

(def input
  [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,1,23,6,27,1,6,27,31,1,13,31,35,1,13,35,39,1,39,13,43,2,43,9,47,2,6,47,51,1,51,9,55,1,55,9,59,1,59,6,63,1,9,63,67,2,67,10,71,2,71,13,75,1,10,75,79,2,10,79,83,1,83,6,87,2,87,10,91,1,91,6,95,1,95,13,99,1,99,13,103,2,103,9,107,2,107,10,111,1,5,111,115,2,115,9,119,1,5,119,123,1,123,9,127,1,127,2,131,1,5,131,0,99,2,0,14,0])

(defn mutate
  [program, ip]
  "Returns updated program"
  (let [ [opcode op1index op2index target] (take 4 (drop ip program))
         op1 (get program op1index)
         op2 (get program op2index) ]
    (case opcode 
      1 (assoc program target (+ op1 op2))
      2 (assoc program target (* op1 op2)))))

(defn run
  [program, ip]
  "Runs the program to completion"
  (loop [program program ip ip]
    (let [ opcode (get program ip) ]
      (if (= opcode 99)
        (get program 0)
        (recur (mutate program ip) (+ ip 4))))))

(defn run-with-input
  [noun, verb]
  "Runs the program with given input"
  (-> input
    (assoc 1 noun)
    (assoc 2 verb)
    (run 0)))

(def part1 (run-with-input 12 2))

(def part2 
  (for [noun (range 100) 
        verb (range 100)
        :let [result (run-with-input noun verb)] 
        :when (= result 19690720)] 
        (+ (* 100 noun) verb)))

(defn -main
  [& args]
  (println part1 part2))
