(ns aoc23.day08
  (:require [clojure.string :as str]))

(def testf "data/day08-test.txt")
(def test2f "data/day08-test2.txt")
(def inputf "data/day08-input.txt")

(defn parse-line
  "Parse each line of the graph"
  [line]
  (let [[_ node l r] (first (re-seq #"^([A-Z]+)\s=\s\(([A-Z]+),\s([A-Z]+)\)" line))]
    {node [l r]}))

(defn read-data
  "Read in the data"
  [f]
  (let [raw-data (str/split (slurp f) #"\n\n")
        graph (str/split-lines (second raw-data))]
    {:path (first raw-data)
     :graph (apply conj (map parse-line graph))}))

(defn traverse-graph
  [{:keys [path graph]}]
  (loop [posn "AAA"
         path (flatten (repeat (str/split path #"")))
         steps 1]
    (let [posn' (if (= "L" (first path))
                  (first (get graph posn))
                  (second (get graph posn)))]
      (if (= "ZZZ" posn')
        steps
        (recur posn' (rest path) (inc steps))))))

(defn part1
  [f]
  (let [data (read-data f)]
    (traverse-graph data)))

;; The End