(ns aoc-2022.day6)

(defn solve [input different-digits-count]
  (->> input
       (map-indexed (fn [i item] [item (inc i)]))
       (partition different-digits-count 1)
       (drop-while
        (fn [group]
          (< (count (set (map first group))) different-digits-count)))
       first))

(def input (slurp "./inputs/day6.txt"))
(println "part 1: " (solve input 4))
(println "part 2: " (solve input 14))
