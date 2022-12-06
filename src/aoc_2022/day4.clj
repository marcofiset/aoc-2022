(ns aoc-2022.day4
  (:require [clojure.string :as str]))

(defn parse-range [range]
  (mapv parse-long (str/split range #"-")))

(defn parse-line [line]
  (let [ranges (str/split line #",")]
    (map parse-range ranges)))

(defn completely-overlaps [[start1 end1] [start2 end2]]
  (and (>= start1 start2)
       (<= end1 end2)))

(defn range-encloses-other [range1 range2]
  (or (completely-overlaps range1 range2)
      (completely-overlaps range2 range1)))

(defn ranges-overlap [[start1 end1] [start2 end2]]
  (and (<= start1 end2)
       (>= end1 start2)))

(defn solve [strategy input]
  (->> input
       str/split-lines
       (map parse-line)
       (filter (partial apply strategy))
       count))

(defn part-1 [input]
  (solve range-encloses-other input))

(defn part-2 [input]
  (solve ranges-overlap input))

(def input (slurp "./inputs/day4.txt"))
(part-1 input)
(part-2 input)
