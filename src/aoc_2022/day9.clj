(ns aoc-2022.day9
  (:require [clojure.string :as str]))

(def input (slurp "./inputs/day9.txt"))

(defn parse-moves [input]
  (->> input
       str/split-lines
       (map #(str/split % #" "))))

(defn touching? [head tail]
  (not-any? #(> % 1) (map (comp abs -) head tail)))

;; Had to google how to do this in a concise manner
;; https://twitter.com/jaffathecake/status/1296382880030044160/photo/1
;; 😂
(defn normalize
  [value]
  (min (max value -1) 1))

(defn next-tail-pos [tail head]
  (if (empty? tail)
    []
    (let [[knot & remaining] tail]
      (if (touching? knot head)
        tail
        (let [move (map (comp normalize -) head knot)
              new-pos (map + knot move)]
          (concat [new-pos] (next-tail-pos remaining new-pos)))))))

(defn execute-move [state [dir n]]
  (let [n (parse-long n)
        moves (repeat n (case dir
                          "R" [1 0]
                          "L" [-1 0]
                          "U" [0 1]
                          "D" [0 -1]))]
    (reduce (fn [state move]
              (let [new-head (mapv + move (:head state))
                    new-tail (next-tail-pos (:tail state) new-head)]
                (-> state
                    (assoc :head new-head)
                    (assoc :tail new-tail)
                    (update :visited-positions conj (last new-tail)))))
            state moves)))

(defn part-1 []
  (->> input
       parse-moves
       (reduce execute-move {:head [0 0] :tail (repeat 9 [0 0]) :visited-positions #{[0 0]}})
       :visited-positions
       count))

(println "part-1:" (pr-str (part-1)))
