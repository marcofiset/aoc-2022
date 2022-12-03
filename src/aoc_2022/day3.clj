(ns aoc-2022.day3
  (:require [clojure.string :as str]
            [clojure.set :refer [intersection]]))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def item-priorities (zipmap (concat (char-range \a \z)
                                     (char-range \A \Z))
                             (iterate inc 1)))

(defn split-halves [str]
  (let [half-point (/ (count str) 2)]
    [(take half-point str) (drop half-point str)]))

(defn find-common-item [item-lists]
  (first (apply intersection (map set item-lists))))

(defn part-1 [input]
  (->> input
       str/split-lines
       (transduce (map (comp item-priorities find-common-item split-halves)) + 0)))

(defn part-2 [input]
  (->> input
       str/split-lines
       (partition-all 3)
       (transduce (map (comp item-priorities find-common-item)) + 0)))

(def input (slurp "./inputs/day3.txt"))
(part-1 input)
(part-2 input)
