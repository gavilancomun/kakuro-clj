(ns kakuro.core
  (:require [clojure.pprint :as pp])
  (:require [clojure.spec :as s]))

(s/instrument-all)

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

(def v (->Value #{1 2 3 4 5 6 7 8 9}))
(def e (->Empty))
(defn d [n] (->Down n))
(defn a [n] (->Across n))
(defn da [d a] (->DownAcross d a))

(s/fdef all-different
        :args (s/cat :nums (s/spec (s/* integer?)))
        :ret boolean?)

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

(defn cell? [v]
  (instance? kakuro.core.Cell v))

(s/def ::cells (s/coll-of cell? []))

(defn transpose [m]
  (apply (partial mapv vector) m))

(s/fdef solve-step
        :args (s/cat :cells ::cells
                     :total integer?))

(defn solve-step [cells total]
  (let [final (dec (count cells))
        perms (->> (permute-all cells total)
         (filter #(is-possible? (get cells final) (get % final)))
         (filter all-different))]
    (->> perms
         transpose
         (map #(->Value (into #{} %))))))

(defn solve-pair [f pair]
  (let [[nvs vs] pair
        target (f (last nvs))]
    (if (seq vs)
      (concat nvs (solve-step (into [] vs) target))
      nvs)))

(defn gather-values [line]
  (partition-by #(= (type %) (type v)) line))

(defn pair-target-with-values [line]
  (partition-all 2 (gather-values line)))

(defn solve-line [line pair-solver]
  (let [pairs (pair-target-with-values line)]
    (into [] (mapcat pair-solver pairs))))

(s/fdef solve-row :args (s/cat :row ::cells))

(defn solve-row [row]
  (solve-line row #(solve-pair :across %)))

(s/fdef solve-column :args (s/cat :column ::cells))

(defn solve-column [column]
  (solve-line column #(solve-pair :down %)))

(defn solve-grid [grid]
  (->> grid
       (mapv solve-row)
       transpose
       (mapv solve-column)
       transpose))

(defn solver [grid]
  (s/instrument-all)
  (let [g (solve-grid grid)]
    (if (= g grid)
      g
      (solver g))))

