(ns kakuro.core)

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

(def val9 (->Value #{1 2 3 4 5 6 7 8 9}))
(def e (->Empty))
(defn d [n] (->Down n))
(defn a [n] (->Across n))
(defn da [d a] (->DownAcross d a))

(defn create-values [n]
  (->> (range 1 (+ 1 n))
       (map (fn [_] val9))
       (into [])))

(def grid1 [
            [e (d 4) (d 22) e (d 16) (d 3)]
            [(a 3) val9 val9 (da 16 6) val9 val9 ]
            [(a 18) val9 val9 val9 val9 val9]
            [e (da 17 23) val9 val9 val9 (d 14)]
            [(a 9) val9 val9 (a 6) val9 val9]
            [(a 15) val9 val9 (a 2) val9 val9]
           ])

(defn draw-row [row]
  (str (apply str (map draw row)) "\n"))

(defn draw-grid [grid] (apply str (map draw-row grid)))
