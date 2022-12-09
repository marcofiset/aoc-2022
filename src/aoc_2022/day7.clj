(ns aoc-2022.day7
  (:require [clojure.string :as str]))

(defn is-command-output [line]
  (not= \$ (first line)))

(defn parse-command [command output]
  (let [[_ command args] (re-find #"\$ (\w+) ?(.*)?" command)]
    {:command command
     :args args
     :output output}))

(defn parse-commands [[command & output]]
  (loop [current-command command
         terminal-output output
         parsed-commands []]
    (if (empty? terminal-output)
      parsed-commands
      (let [output (take-while is-command-output terminal-output)
            [next-command & remaining-ouput] (drop-while is-command-output terminal-output)
            parsed-command (parse-command current-command output)]
        (recur next-command remaining-ouput (conj parsed-commands parsed-command))))))

(defmulti execute-command (fn [_ {:keys [command]}] command))

(defmethod execute-command "cd" [state {:keys [args]}]
  (case args
    "/" (assoc state :current-path ["/"])
    ".." (update state :current-path pop)
    (update state :current-path conj args)))

(defn parse-file [line]
  (let [[size name] (str/split line #" ")]
    [name (if (= "dir" size) [] (parse-long size))]))

(defmethod execute-command "ls" [{:keys [current-path] :as state} {:keys [output]}]
  (let [contents (into {} (map parse-file output))]
    (assoc-in state (concat [:file-tree] current-path) contents)))

(defmethod execute-command :default [_ _]
  (throw (Exception. "unsupported command")))

(defn walk-files
  ([file-tree f]
   (walk-files file-tree [] f))
  ([file-tree current-path f]
   (doseq [[name contents] file-tree
           :let [path (conj current-path name)]]
     (if (map? contents)
       (walk-files contents path f)
       (f path contents)))))

(defn list-files [file-tree]
  (let [files (volatile! {})]
    (walk-files file-tree (fn [path size]
                            (vswap! files assoc path size)))
    @files))

(defn create-file-tree [input]
  (->> (str/split-lines input)
       parse-commands
       (reduce execute-command {:file-tree {} :current-path []})
       :file-tree))

(defn calculate-directory-sizes [directories [path size]]
  (if-let [parents (butlast path)]
    (reduce (partial merge-with +)
            directories
            (map (fn [parents] {parents size}) (partition-all (count parents) 1 (reverse parents))))
    directories))

(defn parse-directory-sizes [input]
  (->> input
       create-file-tree
       list-files
       (reduce calculate-directory-sizes {})))

(defn part-1 [input]
  (let [directory-sizes (parse-directory-sizes input)]
    (->> directory-sizes
         vals
         (filter #(<= % 100000))
         (reduce + 0))))

(defn part-2 [input]
  (let [directory-sizes (parse-directory-sizes input)
        used-space (directory-sizes ["/"])
        remaining-space (- 70000000 used-space)]
    (->> directory-sizes
         vals
         sort
         (drop-while (fn [size]
                       (let [new-remaining (+ remaining-space size)]
                         (< new-remaining 30000000))))
         first)))

(def input (slurp "./inputs/day7.txt"))
(println "part-1:" (part-1 input))
(println "part-2:" (part-2 input))
