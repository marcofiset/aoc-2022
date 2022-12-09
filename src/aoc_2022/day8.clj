(ns aoc-2022.day8
  (:require [clojure.string :as str]))

(defn parse-trees [input]
  (->> input
       str/split-lines
       (mapv #(mapv parse-long (str/split % #"")))))

(def input (slurp "./inputs/day8.txt"))
(def trees (parse-trees input))
(def coords (for [y (range (count trees))
                  x (range (count (first trees)))]
              [x y]))

(defn tree-at [trees [x y]]
  (get (get trees y) x))

(defn take-trees [all-trees from dir]
  (loop [point (map + from dir)
         trees []]
    (if-let [tree (tree-at all-trees point)]
      (recur (map + point dir) (conj trees tree))
      trees)))

(defn on-edge [trees [x y]]
  (or (= x 0)
      (= y 0)
      (= x (dec (count (first trees))))
      (= y (dec (count trees)))))

(defn blocks-sight [tree trees]
  (some #(>= % tree) trees))

(defn compute-line-of-sights [trees from]
  (let [directions [[0 1] [0 -1] [1 0] [-1 0]]]
    (map #(take-trees trees from %) directions)))

(defn is-visible [coords trees]
  (or (on-edge trees coords)
      (let [tree (tree-at trees coords)]
        (not-every? (partial blocks-sight tree) (compute-line-of-sights trees coords)))))

(defn filter-visible [trees]
  (filter #(is-visible % trees) coords))

(defn scenic-score [trees coord]
  (let [tree-value (tree-at trees coord)]
    (->> (compute-line-of-sights trees coord)
         (map #(reduce (fn [n tree]
                         (if (>= tree tree-value)
                           (reduced (inc n))
                           (inc n)))
                       0 %))
         (reduce *))))

(defn part-1 []
  (-> trees
      filter-visible
      count))

(defn part-2 []
  (->> coords
      (map (partial scenic-score trees))
      (apply max)))

(println "part-1:" (part-1))
(println "part-2:" (part-2))
