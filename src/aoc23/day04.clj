(ns aoc23.day04
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [instaparse.core :as insta]))

(def testf "data/day04-test.txt")
(def inputf "data/day04-input.txt")

(def parse-cards
  (insta/parser
   "<cards> := (card <newline>)+
    card := <card-id> <':'> winning <' |'> chosen
    card-id := <'Card'> <space>+ number
    winning := (<space> number)+
    chosen := (<space> number)+
    number := #'\\d+'
    <space> := #'\\s+'
    newline := '\\n'"))

(defn transform-data
  [data]
  #_{:clj-kondo/ignore [:unresolved-var]}
  (insta/transform
   {:number edn/read-string
    :card #(apply conj %&)
    :winning #(hash-map :winning %&)
    :chosen #(hash-map :chosen %&)}
   data))

(defn score-card
  "Count the numbers of chosen numbers in the winning list"
  [{:keys [winning chosen]}]
  (count 
   (set/intersection (set winning) (set chosen))))

(defn add-cards
  "Add additional subsequent cards based on the score"
  [nums index scores]
  (let [score (nth scores index)
        n (nth nums index)
        delta (concat (repeat (inc index) 0)
                      (repeat score n)
                      (repeat (- (count scores) (+ score (inc index))) 0))]
    (map + nums delta)))

(defn process-cards
  "Process the cards in order, adding cards according to the scores"
  [scores]
  (let [len (count scores)
        rng (range len)]
   (reduce (fn [acc i] 
             (add-cards acc i scores))
          (repeat len 1)
          rng)))

(defn part1
  [f]
  (let [data (slurp f)]
    (->> data
         parse-cards
         transform-data
         (map score-card)
         (map #(int (Math/pow 2 (dec %))))
         (apply +))))

(defn part2
  [f]
  (let [data (slurp f)]
    (->> data
         parse-cards
         transform-data
         (map score-card)
         process-cards
         (apply +))))

;; The End