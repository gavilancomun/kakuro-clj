(ns kakuro.test
  (:require [clojure.test :refer :all]
            [kakuro.core :refer :all]))

(def grid1 [[(e) (d 4) (d 22) (e) (d 16) (d 3)]
            [(a 3) (v) (v) (da 16 6) (v) (v)]
            [(a 18) (v) (v) (v) (v) (v)]
            [(e) (da 17 23) (v) (v) (v) (d 14)]
            [(a 9) (v) (v) (a 6) (v) (v)]
            [(a 15) (v) (v) (a 12) (v) (v)]])

(def grid2 [[(e) (d 23) (d 30) (e) (e) (d 27) (d 12) (d 16)]
            [(a 16) (v) (v) (e) (da 17 24) (v) (v) (v)]
            [(a 17) (v) (v) (da 15 29) (v) (v) (v) (v)]
            [(a 35) (v) (v) (v) (v) (v) (d 12) (e)]
            [(e) (a 7) (v) (v) (da 7 8) (v) (v) (d 7)]
            [(e) (d 11) (da 10 16) (v) (v) (v) (v) (v)]
            [(a 21) (v) (v) (v) (v) (a 5) (v) (v)]
            [(a 6) (v) (v) (v) (e) (a 3) (v) (v)]
            ])

(deftest draw-empty
  (is (= "   -----  " (draw (e)))))

(deftest draw-across
  (is (= "   --\\ 5  " (draw (a 5)))))

(deftest draw-down
  (is (= "    4\\--  " (draw (d 4)))))

(deftest draw-down-across
  (is (= "    3\\ 4  " (draw (da 3 4)))))

(deftest draw-values
  (is (= " 123456789" (draw (v))))
  (is (= " 12......." (draw (v 1 2)))))

(deftest drawrow
  (let [line [(da 3 4) (v) (v 1 2) (d 4) (e) (a 5) (v 4) (v 1)]
        result (draw-row line)]
    (is (= "    3\\ 4   123456789 12.......    4\\--     -----     --\\ 5       4         1    \n" result))))

(deftest products
  (let [data [[1 2] [10] [100 200 300]]
        expected [[1 10 100] [1 10 200] [1 10 300] [2 10 100] [2 10 200] [2 10 300]]]
    (is (= expected (product data)))))

(deftest products2
  (let [data [[1 2] [10] [100 200 300]]
        expected [[1 10 100] [1 10 200] [1 10 300] [2 10 100] [2 10 200] [2 10 300]]]
    (is (= expected (product-r data)))))

(deftest permutes
  (let [vs [(v) (v) (v)]
        results (permute-all 6 vs)
        diff (filter all-different results)]
    (println results)
    (is (= 10 (count results)))
    (is (= 6 (count diff)))))

(deftest transposes
  (let [ints [[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]]
        tr (transpose ints)]
    (is (= (count ints) (count (first tr))))
    (is (= (count (first ints)) (count tr)))))

(deftest transposes-r
  (let [ints [[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]]
        tr (transpose-r ints)]
    (is (= (count ints) (count (first tr))))
    (is (= (count (first ints)) (count tr)))))

(deftest isposs
  (let [vc (v 1 2 3)]
    (is (= true (is-possible? vc 2)))
    (is (= false (is-possible? vc 4)))))

(deftest value-equality
  (is (= (v) (v)))
  (is (= (v 1 2) (v 1 2))))

(deftest test-solvestep
  (let [result (solve-step 5 [(v 1 2) (v)])]
    (print "solve step result ")
    (println result)
    (is (= (v 1 2) (first result)))
    (is (= (v 3 4) (second result)))))

(deftest test-gather
  (let [line [(da 3 4) (v) (v) (d 4) (e) (a 4) (v) (v)]
        result (into [] gather-values line)]
    (print "gather ")
    (println result)
    (is (= 4 (count result)))
    (is (= (da 3 4) (-> result first first)))
    (is (= (d 4) (-> result rest rest first first)))
    (is (= (e) (-> result rest rest first second)))
    (is (= (a 4) (-> result rest rest first rest rest first)))))

(deftest test-pairtargets
  (let [line [(da 3 4) (v) (v) (d 4) (e) (a 4) (v) (v)]
        result (into [] pair-targets-with-values line)]
    (print "pair ")
    (println result)
    (is (= 2 (count result)))
    (is (= (da 3 4) (->> result first first first)))
    (is (= (d 4) (->> result second first first)))
    (is (= (e) (->> result second first second)))
    (is (= (a 4) (->> result second first rest rest first)))))

(deftest test-solvepair
  (let [line [(da 3 4) (v) (v) (d 4) (e) (a 4) (v) (v)]
        pairs (into [] pair-targets-with-values line)
        pair (first pairs)
        result (solve-pair :down pair)]
    (print "solvePair ")
    (println result)
    (is (= 3 (count result)))
    (is (= (v 1 2) (second result)))
    (is (= (v 1 2) (nth result 2)))))

(def test-solveline
  (let [line [(da 3 4) (v) (v) (d 4) (e) (a 5) (v) (v)]
        result (into [] (solve-line :across) line)]
    (print "solve line ")
    (println result)
    (is (= 8 (count result)))
    (is (= (v 1 3) (second result)))
    (is (= (v 1 3) (nth result 2)))
    (is (= (v 1 2 3 4) (nth result 6)))
    (is (= (v 1 2 3 4) (nth result 7)))))

(deftest test-row
  (let [result (solve-row [(a 3) (v 1 2 3) (v 1)])]
    (is (= (v 2) (second result)))
    (is (= (v 1) (nth result 2)))))

(deftest test-col
    (let [result (solve-column [(da 3 12) (v 1 2 3) (v 1)])]
    (is (= (v 2) (second result)))
    (is (= (v 1) (nth result 2)))))

(deftest test-grid
  (let [result (solver grid1)]
    (is (= "   --\\ 3       1         2       16\\ 6       4         2    \n" (draw-row (second result))))
    (is (= "   --\\18       3         5         7         2         1    \n" (draw-row (nth result 2))))
    (is (= "   -----     17\\23       8         9         6       14\\--  \n" (draw-row (nth result 3))))
    (is (= "   --\\ 9       8         1       --\\ 6       1         5    \n" (draw-row (nth result 4))))
    (is (= "   --\\15       9         6       --\\12       3         9    \n" (draw-row (nth result 5))))))

(deftest test-grid2
  (let [result (-> grid2 solver draw-grid)
        expected "     2         1    \n"]
    (is (= expected (.substring result (- (count result) (count expected)))))))

