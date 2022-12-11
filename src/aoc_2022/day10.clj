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

(defn- compute-cycles [operations]
  (let [cycle-count   (count-cycles operations)
        initial-state {:cycles (vec (repeat cycle-count 1)) :index 0}]
    (->> operations
         (reduce execute-operation initial-state)
         :cycles)))

(defn part-1 [input]
  (let [operations (parse-operations input)]
    (->> (compute-cycles operations)
         (map-indexed (fn [i value] (* value (inc i))))
         (drop 19)
         (take-nth 40)
         (reduce +))))

(defn draw-line [line]
  (map-indexed
   (fn [i x]
     (if (<= (abs (- i x)) 1)
       "#" "."))
   line))

(defn part-2 [input]
  (let [cycles (-> input parse-operations compute-cycles)]
    (->> cycles
         (partition 40)
         (map draw-line)
         (map #(str/join "" %))
         (str/join "\n"))))

(println "part-1:" (part-1 input))
(println "part-2:\n" (part-2 input))
