(ns aoc23.day10
  (:require [aoc23.util :as util]))

(def testf "data/day10-test.txt")
(def test2f "data/day10-test2.txt")
(def test3f "data/day10-test3.txt")
(def inputf "data/day10-input.txt")

(defn find-start-node
  "Extract the coordinates of each character in the matrix than is not a period."
  [data]
  (for [x (range 0 (count (first data)))
        y (range 0 (count data))
        :when (= \S (get-in data [x y]))]
    [x y]))

(defn navigate-loop
  "Navigate the loop and update the position based on the character and the entry direction"
  [data rc [dr dc]]
  (let [val (get-in data rc)
        dir' (case val
               \F (if (zero? dr) [1 0] [0 1])
               \7 (if (zero? dr) [1 0] [0 -1])
               \J (if (zero? dr) [-1 0] [0 -1])
               \L (if (zero? dr) [-1 0] [0 1])
               \| [dr dc]
               \- [dr dc]
               \S [dr dc]
               nil)]
    {:rc (mapv + rc dir') :dir dir'}))

(defn find-first-step
  [[r c :as start]]
  (cond
    (= [2 0] start) {:rc [2 0] :dir [0 1]}
    (= [62 111] start) {:rc [62 111] :dir [-1 0]} ;; taken from the input data
    :else {:rc [(inc r) c] :dir [0 1]}))

(defn traverse-loop
  [data]
  (let [start (first (find-start-node data))]
    (set! *print-length* 5) ; help the debugger
    (loop [{:keys [rc dir]} (find-first-step start)
           length 0]
      (let [state (navigate-loop data rc dir)]
        (if (= (:rc state) start)
          (inc length)
          ;; else
          (recur state (inc length)))))))

(defn part1
  [f]
  (let [data (util/import-data f)]
    (-> data
        traverse-loop
        (quot 2))))

;; The End