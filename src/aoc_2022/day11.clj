(ns aoc-2022.day11
  (:require [clojure.string :as str]))

(defn parse-operation [operation]
  (let [[term1 operator term2] (str/split operation #" ")]
    (fn [value]
      ((case operator "+" + "*" *)
       (case term1 "old" value (parse-long term1))
       (case term2 "old" value (parse-long term2))))))

(defn parse-monkey [monkey-input]
  (let [[_ starting-items operation dividor true-index false-index] monkey-input]
    {:items        (map parse-long (str/split starting-items #", "))
     :operation    (parse-operation operation)
     :divisible-by (parse-long dividor)
     :true-index   (parse-long true-index)
     :false-index  (parse-long false-index)
     :inspected-items-count 0}))

(def input (slurp "./inputs/day11.txt"))

(def monkey-strings
  (re-seq #"Monkey \d:\s+Starting items: (?<items>(?:\d+,? ?)+)
\s+Operation: new = (?<operation>[^\n]+)
\s+Test: divisible by (?<dividor>\d+)
\s+If true: throw to monkey (?<trueindex>\d)
\s+If false: throw to monkey (?<falseindex>\d)" input))

(def monkeys (mapv parse-monkey monkey-strings))

(defn throw-item [monkeys from to value]
  (-> monkeys
      (update-in [from :items] (comp vec (partial drop 1)))
      (update-in [to :items] conj value)))

(defn evaluate-and-throw-items [worry-reducer monkeys i]
  (let [{:keys [items operation divisible-by true-index false-index]} (get monkeys i)]
    (reduce (fn [monkeys item]
              (let [new-item-value (worry-reducer (operation item))
                    target (if (zero? (mod new-item-value divisible-by))
                             true-index false-index)]
                (-> monkeys
                    (throw-item i target new-item-value)
                    (update-in [i :inspected-items-count] inc))))
            monkeys
            items)))

(defn play-round [worry-reducer monkeys _]
  (reduce (partial evaluate-and-throw-items worry-reducer) monkeys (range (count monkeys))))

(defn solve [reduce-worry? monkeys iterations]
  (let [worry-reducer (if reduce-worry? #(quot % 3) identity)]
    (->> (reduce (partial play-round worry-reducer) monkeys (range iterations))
         (map :inspected-items-count)
         (sort >)
         (take 2)
         (reduce *))))

(println "part-1:" (solve true monkeys 20))
(println "part-2:" (solve false monkeys 10000))
