(ns aoc-2022.day12
  (:require [clojure.string :as str]))

(def input (slurp "./inputs/day12.txt"))
(def grid (->> input str/split-lines (mapv #(into [] %))))

(defn find-points [grid needle]
  (reduce (fn [points [y row]]
            (let [x (.indexOf row needle)]
              (if (= -1 x)
                points
                (conj points [x y]))))
          []
          (map-indexed vector grid)))

(defn cell [grid [x y]]
  (get (get grid y) x))

(defn neighbors [grid [x y]]
  (->> [[(dec x) y]
        [(inc x) y]
        [x (dec y)]
        [x (inc y)]]
       (remove #(nil? (cell grid %)))))

(defn valid-neighbors [grid point]
  (let [current (cell grid point)
        current (case current \S \a current)]
    (->> (neighbors grid point)
         (filter (fn [coord]
                   (let [neighbor (cell grid coord)
                         neighbor (case neighbor \E \z neighbor)]
                     (<= (- (int neighbor) (int current)) 1)))))))

(defn shortest-path [grid start end]
  (loop [to-visit (conj clojure.lang.PersistentQueue/EMPTY start)
         seen {start 0}]
    (let [current (peek to-visit)
          current-length (seen current)
          rest (pop to-visit)
          neighbors (->> (valid-neighbors grid current)
                         (remove #(contains? seen %)))]
      (if (= current end)
        current-length
        (recur (apply conj rest neighbors)
               (into seen (zipmap neighbors (repeatedly #(inc current-length)))))))))

(defn part-1 [grid]
  (let [[start] (find-points grid \S)
        [end]   (find-points grid \E)]
    (shortest-path grid start end)))

(defn part-2 [grid]
  (let [possible-starts (find-points grid \a)
        [end] (find-points grid \E)]
    (->> possible-starts
         (map #(shortest-path grid % end))
         (apply min))))

(println "part-1:" (part-1 grid))
(println "part-2:" (part-2 grid))
