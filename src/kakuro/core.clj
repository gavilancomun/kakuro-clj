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

;;(defn transpose [m]
;;  (apply mapv vector m))

(defn transpose-reducing
  ([] [])
  ([acc v]
   (into [] (map-indexed
              (fn [i x] (vec (conj (get acc i) x))))
         v)))

(defn transpose [m]
  (reduce transpose-reducing [] m))

(defn ^:private preserving-reduced
  [rf]
  #(let [ret (rf %1 %2)]
     (if (reduced? ret)
       (reduced ret)
       ret)))

(defn create-transducer [f]
  (fn [rf]
    (let [my-result (atom (f))
          rrf (preserving-reduced rf)]
      (fn 
        ([] (rf))
        ([result]
         (reduce rrf result @my-result))
        ([result input] 
         (swap! my-result f input)
         result)))))

(def transpose-r 
  (create-transducer transpose-reducing))

(defn permute-reducing 
  ([] [])
  ([acc v]
   (if (= 0 (count acc))
     (mapv vector v)
     (for [x acc
           y v]
       (conj (vec x) y)))))

(defn permute-all [colls]
  (reduce permute-reducing [] colls))

(def permute-all-r
  (create-transducer permute-reducing))

(defn solve-step [total cells]
  (let [final (dec (count cells))
        final-cell (get cells final)]
    (->> cells 
         (map :values)
         permute-all
         (filter #(= total (apply + %)))
         (filter all-different)
         transpose
         (map #(->Value (into #{} %))))))

(defn solve-pair [f [nvs vs]]
  (if (seq vs)
    (concat nvs (solve-step (f (last nvs)) (into [] vs)))
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

