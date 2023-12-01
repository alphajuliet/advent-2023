(ns aoc23.day01-test
  (:require [clojure.test :refer [deftest is]]
            [aoc23.day01 :as subject]))

(deftest get-numbers-2-test
  (is (= 23 (subject/make-number (subject/get-numbers-words "twovgtprdzcjjzkq3ffsbcblnpq"))))
  (is (= 27 (subject/make-number (subject/get-numbers-words "two8sixbmrmqzrrb1seven"))))
  (is (= 71 (subject/make-number (subject/get-numbers-words "7fvfourgkfkkbloneeightdrfscspgkdrmzzt1"))))
  (is (= 12 (subject/make-number (subject/get-numbers-words "132"))))
  (is (= 14 (subject/make-number (subject/get-numbers-words "zoneight234"))))
  (is (= 76 (subject/make-number (subject/get-numbers-words "7pqrstsixteen"))))
  (is (= 84 (subject/make-number (subject/get-numbers-words "heightwothreennzljtptwo94"))))
  (is (= 38 (subject/make-number (subject/get-numbers-words "3eightwoqs"))))
  )