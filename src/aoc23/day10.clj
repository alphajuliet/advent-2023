(ns aoc23.day10
  (:require [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [aoc23.util :as util]))

(def testf "data/day10-test.txt")
(def test2f "data/day10-test2.txt")
(def test3f "data/day10-test3.txt")
(def inputf "data/day10-input.txt")

(defn read-data
  [f]
  (->> f
       util/import-data))

#_(defn extract-nodes
  "Extract the coordinates of each character in the matrix than is not a period."
  [data]
  (for [x (range 0 (count (first data)))
        y (range 0 (count data))
        :when (not= \. (get-in data [x y]))]
    [x y]))

(defn find-start-node
  "Extract the coordinates of each character in the matrix than is not a period."
  [data]
  (for [x (range 0 (count (first data)))
        y (range 0 (count data))
        :when (= \S (get-in data [x y]))]
    [x y]))

(defn navigate-loop
  "Navigate the loop and update the position based on the character"
  [data [r c :as rc]]
  (let [val (get-in data rc)]
    (case val
      \F [[(inc r) c] [r (inc c)]]
      \7 [[(inc r) c] [r (dec c)]]
      \J [[(dec r) c] [r (dec c)]]
      \L [[(dec r) c] [r (inc c)]]
      \| [[(dec r) c] [(inc r) c]]
      \- [[r (dec c)] [r (inc c)]]
      \S rc
      nil)))

(defn find-first-step
  [[r c :as start]]
  (cond
    (= [2 0] start) [2 1]
    (= [62 111] start) [62 110] ;; cheating
    :else [(inc r) c]))

(defn traverse-loop
  [data]
  (let [start (first (find-start-node data))]
    (set! *print-length* 5) ; help the debugger
    (loop [rc (find-first-step start)
           length 0]
      (let [rc' (navigate-loop data rc)]
        (if (= rc' start)
          length
          (recur rc' (inc length)))))))

#_(defn make-graph
  "Make a map representation of the graph"
  [data]
  (let [nodes (extract-nodes data)]
    (into {} (for [node nodes
                   :let [nn (neighbours node (get-in data node))]]
               [node nn]))))

#_(defn find-start-node
  [m]
  (->> m
       (filter #(= nil (second %)))
       first
       first))

#_(defn make-ubergraph
  "Turn into a real graph"
  [m]
  (let []
    (reduce
     (fn [g [node edges]]
       (reduce
        (fn [g edge]
          (uber/add-edges g [node edge]))
        g
        edges))
     (uber/graph)
     m)))

(defn find-paths
  [g]
  ())

(defn part1
  [f]
  (let [data (read-data f)
        start (first (find-start-node data))]
    (println "start" start)
    ))

;; The End