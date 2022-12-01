(ns aoc-2022.day1
  (:require [clojure.string :as str]))

(defn parse-elf-foods [elf-string]
  (let [lines (str/split-lines elf-string)]
    (map read-string lines)))

(defn parse-input [input]
  (let [elves-foods (str/split input #"\n\n")]
    (map parse-elf-foods elves-foods)))

(defn sum-elf-foods [elves]
  (map (partial reduce +) elves))

(defn part-1 [input]
  (->> input
       parse-input
       sum-elf-foods
       (sort >)
       first))

(defn part-2 [input]
  (->> input
       parse-input
       sum-elf-foods
       (sort >)
       (take 3)
       (reduce +)))

(def input (slurp "./day1-input.txt"))
(part-1 input)
(part-2 input)
