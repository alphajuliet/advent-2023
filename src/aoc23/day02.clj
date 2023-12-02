(ns aoc23.day02
  (:require [aoc23.numerimap :as n]
            [instaparse.core :as insta]
            [clojure.edn :as edn]))

(def testf "data/day02-test.txt")
(def inputf "data/day02-input.txt")

(def game-parser
  ;; "Parse the log file, e.g. |Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green|"
  (insta/parser
    "<log> := game+
    game := <'Game'> <space> number <': '> sample (<'; '> sample)* <newline>
    sample := cubes (<', '> cubes)*
    cubes := number <space> colour
    number := #'\\d+'
    colour := #'blue|red|green'
    space := #'\\s+'
    newline := '\\n'"))

(defn transform-data
  [data]
  #_{:clj-kondo/ignore [:unresolved-var]}
  (insta/transform
   {:number edn/read-string
    :sample conj
    :colour keyword
    :cubes #(hash-map %2 %1)}
   data))

(defn all-positive?
  "Are all the values in all maps positive?"
  [samples]
  (->> samples
       (map vals)
       flatten
       (every? nat-int?)))

(defn possible?
  "Does every sample in the game have no more than the bag contents?"
  [game]
  (let [bag {:red 12 :green 13 :blue 14}]
    (->> (drop 2 game)
         (map #(n/m-sub bag %))
         all-positive?)))

(defn min-cubes
  [game]
  (->> (drop 2 game)
       (apply merge-with max)))

(defn part1
  [f]
  (->> f 
       slurp
       game-parser
       transform-data
       (map-indexed #(if (possible? %2) (inc %1) 0))
       (apply +)))

(defn part2
  [f]
  (->> f
       slurp
       game-parser
       transform-data
       (map min-cubes)
       (map vals)
       (map #(apply * %))
       (apply +)))

(comment
  (assert (= 8 (part1 testf)))
  (assert (= 2286 (part2 testf))))

;; The End