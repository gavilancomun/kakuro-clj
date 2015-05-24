(ns kakuro.core
  (:require [clojure.pprint :as pprint]))

(defprotocol Cell
  (draw [this]))

(defrecord Empty[]
  Cell
  (draw [this] "   -----  "))

(defrecord Down[down]
  Cell
  (draw [this] (format "   %2d\\--  " (:down this))))

(defrecord Across[across]
  Cell
  (draw [this] (format "   --\\%2d  " (:across this))))

(defrecord DownAcross[down across]
  Cell
  (draw [this] (format "   %2d\\%2d  " (:down this) (:across this))))

(defrecord Value[values]
  Cell
  (draw [this]
    (let [values (:values this)]
      (if (= 1 (count values))
        (first (map #(str "     " % "    ") values))
        (str " " (->> (range 1 10) 
                      (map #(if (contains? values %) % "."))
                      (apply str)))))))

(defn draw-row [row]
  (str (apply str (map draw row)) "\n"))

(defn draw-grid [grid]
  (apply str (map draw-row grid)))

(def v (->Value #{1 2 3 4 5 6 7 8 9}))
(def e (->Empty))
(defn d [n] (->Down n))
(defn a [n] (->Across n))
(defn da [d a] (->DownAcross d a))

(defn all-different [nums]
  (= (count nums) (count (into #{} nums))))

(defn permute [vs target so-far]
  (if (>= target 1)
    (if (= (count so-far) (dec (count vs)))
      [(conj so-far target)]
      (->> (get vs (count so-far))
           :values
           (mapcat #(permute vs (- target %) (conj so-far %)))
           (into [])))
    []))

(defn permute-all [vs total]
  (permute vs total []))

(defn is-possible? [cell n]
  (contains? (:values cell) n))

(defn transpose [m]
  (apply mapv vector m))

(defn solve-step [cells total]
  (let [final (dec (count cells))
        perms (->> (permute-all cells total)
         (filter #(is-possible? (get cells final) (get % final)))
         (filter all-different))]
    (->> perms
         transpose
         (map #(->Value (into #{} %))))))

(defn solve-pair [k [nvs vs]]
  (if (seq vs)
    (concat nvs (solve-step (into [] vs) (k (last nvs))))
    nvs))

(defn solve-line [line pair-solver]
  (let [pairs (partition-all 2 (partition-by #(= (type %) (type v)) line))]
    (into [] (mapcat pair-solver pairs))))

(defn solve-row [row]
  (solve-line row #(solve-pair :across %)))

(defn solve-column [column]
  (solve-line column #(solve-pair :down %)))

(defn solve-grid [grid]
  (->> grid
       (mapv solve-row)
       transpose
       (mapv solve-column)
       transpose))

(defn solver [grid]
  (let [g (solve-grid grid)]
    (if (= g grid)
      g
      (solver g))))

(def grid1 [
            [e (d 4) (d 22) e (d 16) (d 3)]
            [(a 3) v v (da 16 6) v v ]
            [(a 18) v v v v v]
            [e (da 17 23) v v v (d 14)]
            [(a 9) v v (a 6) v v]
            [(a 15) v v (a 12) v v]
           ])

