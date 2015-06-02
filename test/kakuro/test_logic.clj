(ns kakuro.test-logic
    (:require [clojure.test :refer :all]
              [kakuro.logic :refer :all]))

(deftest test-solve1
  (let [result (solve-sum-n 2 6)
        expected [[1 5] [2 4] [4 2] [5 1]]]
    (is (= expected result))))

(deftest test-solve2
  (let [result (solve-sum-n 3 6)
        expected [[1 2 3] [2 1 3] [1 3 2] [3 1 2] [2 3 1] [3 2 1]]]
    (is (= expected result))))

