(ns aoc23.day08
  (:require [clojure.string :as str]))

(def testf "data/day08-test.txt")
(def test2f "data/day08-test2.txt")
(def test3f "data/day08-test3.txt")
(def inputf "data/day08-input.txt")

(defn parse-line
  "Parse each line of the graph"
  [line]
  (let [[_ node l r] (first (re-seq #"^([\w]+)\s=\s\(([\w]+),\s([\w]+)\)" line))]
    {node [l r]}))

(defn read-data
  "Read in the data"
  [f]
  (let [raw-data (str/split (slurp f) #"\n\n")
        graph (str/split-lines (second raw-data))]
    {:path (first raw-data)
     :graph (apply conj (map parse-line graph))}))

(defn- update-posn 
  "Traverse one step based on the next path instruction"
  [dir graph posn]
  (if (= "L" dir)
    (first (get graph posn))
    (second (get graph posn))))

(defn traverse-graph
  "Traverse the graph from the start to the node ZZZ"
  [{:keys [path graph]} start]
  (loop [posn start
         path (cycle (str/split path #""))
         steps 1]
    (let [posn' (update-posn (first path) graph posn)]
      (if (str/ends-with? posn' "Z")
        steps
        ;; else
        (recur posn' (rest path) (inc steps))))))

(defn gcd
  "Greatest common divisor"
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  "Lowest common multiple"
  [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv
  "Find the LCM of a variable number of items"
  [& v]
  (reduce lcm v))

(defn part1
  [f]
  (let [data (read-data f)]
    (traverse-graph data "AAA")))

(defn part2
  [f]
  (let [data (read-data f)]
    (->> data
         (:graph)
         keys
         (filter #(str/ends-with? % "A"))
         (map #(traverse-graph data %))
         (apply lcmv))))

(comment
  (assert (= 6 (part1 test2f)))
  (assert (= 6 (part2 test3f))))

;; The End