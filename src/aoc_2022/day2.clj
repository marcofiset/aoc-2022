(ns aoc-2022.day2
  (:require [clojure.string :as str]))

(def play-scores {:rock     1
                  :paper    2
                  :scissors 3})

(def outcome-scores {:loss 0
                     :draw 3
                     :win  6})

(def part-1-strategy {"X" :rock
                      "Y" :paper
                      "Z" :scissors})

(defn part-2-strategy [my-move opponent-move]
  (let [moves {"X" {:rock     :scissors
                    :paper    :rock
                    :scissors :paper}
               "Y" {:rock     :rock
                    :paper    :paper
                    :scissors :scissors}
               "Z" {:rock     :paper
                    :paper    :scissors
                    :scissors :rock}}]
    (get-in moves [my-move opponent-move])))

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

(defn parse-play [strategy line]
  (let [[move-1 move-2] (str/split line #"\s")
        opponent-move (opponent-move-map move-1)
        my-move (strategy move-2 opponent-move)
        outcome (round-outcome my-move opponent-move)]
    {:my-move my-move
     :opponent-move opponent-move
     :outcome outcome
     :score (+ (outcome-scores outcome) (play-scores my-move))}))

(defn parse-plays [strategy input]
  (->> input
       (str/split-lines)
       (map #(parse-play strategy %))))

(defn calculate-score [input strategy]
  (->> input
       (parse-plays strategy)
       (map :score)
       (reduce +)))

(def input (slurp "./inputs/day2.txt"))
(calculate-score input part-1-strategy)
(calculate-score input part-2-strategy)
