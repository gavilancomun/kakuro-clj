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
        (str "     " (first values) "    ")
        (apply str " " (->> (range 1 10) 
                            (map #(if (contains? values %) % "."))))))))

(defn draw-row [row]
  (str (apply str (map draw row)) "\n"))

(defn draw-grid [grid]
  (apply str (map draw-row grid)))

(defn v [& args]
  (if (= 0 (count args))
    (->Value #{1 2 3 4 5 6 7 8 9})
    (->Value (into #{} args))))

(defn e [] (->Empty))
(defn d [n] (->Down n))
(defn a [n] (->Across n))
(defn da [d a] (->DownAcross d a))

(defn all-different [nums]
  (= (count nums) (count (into #{} nums))))

(defn product [colls]
  (condp = (count colls)
    0 []
    1 (map vector (first colls))
    (let [head (first colls)
          tail-prod (product (rest colls))]
      (into [] (mapcat (fn [x] (map #(into [x] %) tail-prod)) head)))))

(defn permute-all [vs target]
  (let [values (map :values vs)
        products (product values)]
    (into [] (filter #(= target (apply + %)) products))))

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

(defn solve-pair [f [nvs vs]]
  (if (seq vs)
    (concat nvs (solve-step (into [] vs) (f (last nvs))))
    nvs))

(def gather-values
  (partition-by (partial instance? Value)))

(def pair-targets-with-values
  (comp 
    gather-values
    (partition-all 2)))

(defn solve-line [f]
  (comp 
    pair-targets-with-values
    (mapcat (partial solve-pair f))))

(defn solve-row [row]
  (into [] (solve-line :across) row))

(defn solve-column [column]
  (into [] (solve-line :down) column))

(defn solve-grid [grid]
  (->> grid
       (mapv solve-row)
       transpose
       (mapv solve-column)
       transpose))

(defn solver [grid]
  (println (draw-grid grid))
  (let [g (solve-grid grid)]
    (if (= g grid)
      g
      (solver g))))

