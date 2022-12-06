(ns aoc-2022.day5
  (:require [clojure.string :as str]))

(defn parse-stack-line [line]
  (->> line
       (map (partial str/join ""))
       (map (partial re-find #"[A-Z]"))))

(defn populate-stacks [stacks line]
  (mapv (fn [stack item]
          (if (empty? item)
            stack
            (conj stack item)))
        stacks
        (take (count stacks) (concat line (repeat "")))))

(defn parse-stacks [input]
  (let [stack-lines (->> input
                         str/split-lines
                         reverse
                         (drop 1)
                         (map (partial partition-all 4))
                         (map parse-stack-line))
        stack-count (apply max (map count stack-lines))
        initial-stacks (repeat stack-count [])
        stacks (reduce populate-stacks initial-stacks stack-lines)]
    stacks))

(defn parse-instruction [line]
  (let [[_ & groups]    (re-find #"move (\d+) from (\d+) to (\d+)" line)
        [count from to] (map parse-long groups)]
    {:count count
     :from  (dec from)
     :to    (dec to)}))

(defn parse-instructions [input]
  (->> input
       str/split-lines
       (map parse-instruction)))

(defn parse-input [input]
  (let [[stacks-str instructions-str] (str/split input #"\n\n")
        stacks                        (parse-stacks stacks-str)
        instructions                  (parse-instructions instructions-str)]
    [stacks instructions]))

(defn execute-instruction-9000 [stacks {:keys [from to count] :as instr}]
  (if (zero? count)
    stacks
    (recur
      (-> stacks
          (update from pop)
          (update to conj (peek (stacks from))))
      (update instr :count dec))))


(defn execute-instruction-9001 [stacks {:keys [from to count]}]
  (-> stacks
      (update from (partial drop-last count))
      (update to concat (take-last count (stacks from)))))

(defn solve [strategy input]
  (let [[stacks instructions] (parse-input input)]
    (->> instructions
         (reduce strategy stacks)
         (map last)
         (str/join ""))))

(def input (slurp "./inputs/day5.txt"))
(println "part-1" (solve execute-instruction-9000 input))
(println "part-2" (solve execute-instruction-9001 input))
