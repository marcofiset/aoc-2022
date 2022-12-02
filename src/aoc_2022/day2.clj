(ns aoc-2022.day2
  (:require [clojure.string :as str]))

(def play-scores {:rock     1
                  :paper    2
                  :scissors 3})

(def outcome-scores {:loss 0
                     :draw 3
                     :win  6})

(def my-move-map {"X" :rock
                  "Y" :paper
                  "Z" :scissors})

(def opponent-move-map {"A" :rock
                        "B" :paper
                        "C" :scissors})

(defn round-outcome [my-play opponent-play]
  (case [my-play opponent-play]
    [:rock :scissors]  :win
    [:rock :paper]     :loss
    [:scissors :paper] :win
    [:scissors :rock]  :loss
    [:paper :rock]     :win
    [:paper :scissors] :loss
    :draw))

(defn parse-play [line]
  (let [[move-1 move-2] (str/split line #"\s")
        opponent-move (opponent-move-map move-1)
        my-move (my-move-map move-2)
        outcome (round-outcome my-move opponent-move)]
    {:my-move (my-move-map my-move)
     :opponent-move (opponent-move-map opponent-move)
     :outcome outcome
     :score (+ (outcome-scores outcome) (play-scores my-move))}))

(defn parse-plays [input]
  (->> input
      (str/split-lines)
      (map parse-play)))

(defn part-1 [input]
  (->> input
       parse-plays
       (map :score)
       (reduce +)))

(def input (slurp "./inputs/day2.txt"))
(part-1 input)
