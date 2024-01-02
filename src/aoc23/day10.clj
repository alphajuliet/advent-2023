(ns aoc23.day10
  (:require [aoc23.util :as util]))

(def testf "data/day10-test.txt")
(def test2f "data/day10-test2.txt")
(def test3f "data/day10-test3.txt")
(def test4f "data/day10-test4.txt")
(def test5f "data/day10-test5.txt")
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
  [grid rc [dr dc]]
  (let [val (get-in grid rc)
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
  "Traverse the loop from the start position, counting the steps and all the visited positions."
  [grid]
  (let [start (first (find-start-node grid))]
    (set! *print-length* 5) ; help the debugger
    (loop [{:keys [rc dir]} (find-first-step start)
           visited [start]
           length 0]
      (let [state (navigate-loop grid rc dir)]
        (if (= (:rc state) start)
          {:length (inc length), :visited visited}
          ;; else
          (recur state (conj visited (:rc state)) (inc length)))))))

(defn find-crossings
  "Find where the loop crosses a row"
  [loop-rc row maxc]
  (let [xx (set (filter #(= row (first %)) loop-rc))]
    (loop [posns []
           col 0 
           i 0]
      (if (>= col maxc)
        posns
        ;; else
        (if (and (contains? xx [row col])
                 (odd? i))
          (recur (conj posns [row col]) (inc col) (inc i))
          ;; else
          (recur posns (inc col) i)
          )))))

(defn scan-loop
  "Scan the rows of the grid and identify all spaces inside the grid."
  [grid loop-rc]
  (let [maxr (count grid)
        maxc (count (first grid))]
        ;; Scan each row and identify any spaces that are an odd number of crossings of the loop
(for [r (range maxr)
      c (range maxc)
      :let [x (find-crossings loop-rc r maxc)]
      :when true]
    [r c])))

(defn part1
  [f]
  (let [data (util/import-data f)]
    (-> data
        traverse-loop
        (:length)
        (quot 2))))

(defn part2
  [f]
  (let [data (util/import-data f)]
    (-> data
        traverse-loop
        (:visited))))

;; The End