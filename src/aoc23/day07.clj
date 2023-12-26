(ns aoc23.day07
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [aoc23.util :as util]))

(def testf "data/day07-test.txt")
(def test2f "data/day07-test2.txt")
(def inputf "data/day07-input.txt")
(def input2f "data/day07-input2.txt")

(defn read-data
  "Read in the hands and the bets"
  [f]
  (->> f
       util/import-data
       (map #(str/split % #"\s+"))
       (map #(vector (first %) (edn/read-string (second %))))))

(defn compare-lists
  "Compare two items from an ordered collection"
  [a b coll]
  (let [i1 (.indexOf coll a)
        i2 (.indexOf coll b)]
    (compare i1 i2)))

(defn compare-label
  [a b]
  (compare-lists a b [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2]))

(defn compare-type
  [a b]
  (compare-lists a b [:five :four :full-house :three :two-pairs :pair :high-card :nothing]))

(defn compare-cards
  "Compare cards in order using compare-label"
  [s1 s2]
  (loop [[c1 & rest1] s1
         [c2 & rest2] s2]
    (cond
      (and (nil? c1) (nil? c2)) 0
      (nil? c1) -1
      (nil? c2) 1
      :else (let [cmp (compare-label c1 c2)]
              (if (zero? cmp) (recur rest1 rest2) cmp)))))

(defn sort-cards
  "Sort the hand first by frequency and then by key"
  [hand]
  (->> hand
       (sort-by first compare-label)
       (sort-by second >)))

(defn classify-hand
  "Classify the hand into its type and cards"
  [hand]
  (let [freq (frequencies hand)
        type (case (sort > (vals freq))
               [5] :five
               [4 1] :four
               [3 2] :full-house
               [3 1 1] :three
               [2 2 1] :two-pairs
               [2 1 1 1] :pair
               [1 1 1 1 1] :high-card
               :error)]
    [type (sort-cards (vec freq))]))

(defn sort-hands
  "Sort all the hands by type then by the cards"
  [hands]
  (->> hands
       (sort-by first compare-cards)
       (map (juxt (comp classify-hand first) second))
       (sort-by (comp first first) compare-type)))

(defn part1
  [f]
  (let [data (read-data f)
        rank (range 1 (inc (count data)))]
    (->> data 
         sort-hands
         (map second)
         reverse
         (map * rank)
         (apply +))))

(comment 
  (assert (= 6440 (part1 testf)))
  (assert (= 1343 (part1 test2f))))

;; The End