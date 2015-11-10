(ns kakuro.test-logic
    (:require [clojure.test :refer :all]
              [clojure.core.logic :as cl]
              [clojure.core.logic.fd :as fd]
              [kakuro.core :refer :all]
              [kakuro.logic :refer :all]
              [kakuro.test]))

(defn solve-sum-n [n total]
  (if (> n 0)
    (let [vars (repeatedly n cl/lvar)]
      (cl/run* [q]
               (cl/everyg #(fd/in % (apply fd/domain (range 1 10))) vars)
               (nary-plus total vars)
               (fd/distinct vars)      
               (cl/== q vars)))))

(defn solve-cells-vars [cells vars total]
  (let [n (count cells)]
    (if (> n 0)
        (cl/run* [q]
                 (cl/everyg #(fd/in (first %) (apply fd/domain (into (sorted-set) (:values (second %))))) (map vector vars cells))
                 (nary-plus total vars)
                 (fd/distinct vars)      
                 (cl/== q vars)))))

(defn solve-cells [cells total]
  (when (> (count cells) 0)
    (solve-cells-vars cells (repeatedly (count cells) cl/lvar) total)))

(deftest test-solve1
  (let [result (solve-sum-n 2 6)
        expected [[1 5] [2 4] [4 2] [5 1]]]
    (is (= expected result))))

(deftest test-solve2
  (let [result (solve-sum-n 3 6)
        expected [[1 2 3] [2 1 3] [1 3 2] [3 1 2] [2 3 1] [3 2 1]]]
    (is (= expected result))))

(deftest test-solve-cells
  (let [result (solve-cells [(->Value #{1 2 3 4}) (->Value #{1 2 3 4})] 6)
        expected [[2 4] [4 2]]]
    (is (= expected result))))

(deftest test-grid1
  (is (= nil (logic-grid kakuro.test/grid1))))
