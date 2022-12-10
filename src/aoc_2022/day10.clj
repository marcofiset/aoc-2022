(ns aoc-2022.day10
  (:require [clojure.string :as str]))

(def input (slurp "./inputs/day10.txt"))

(defn parse-operations [input]
  (->> input
       str/split-lines
       (map #(str/split % #" "))))

(defn cycle-length [[op _]]
  (case op
    "noop" 1
    "addx" 2))

(defn count-cycles [ops]
  (transduce (map cycle-length) + ops))

(defn execute-operation [{:keys [index cycles] :as state} [op n]]
  (case op
    "addx" (-> state
               (assoc :cycles (concat (take (+ index 2) cycles)
                                      (map (partial + (parse-long n)) (drop (+ index 2) cycles))))
               (update :index + 2))
    "noop" (update state :index inc)))

(defn part-1 [input]
  (let [operations    (parse-operations input)
        cycle-count   (count-cycles operations)
        initial-state {:cycles (vec (repeat cycle-count 1)) :index 0}]
    (->> operations
         (reduce execute-operation initial-state)
         :cycles
         (map-indexed (fn [i value] (* value (inc i))))
         (drop 19)
         (take-nth 40)
         (reduce +))))

(println "part-1:" (part-1 input))
