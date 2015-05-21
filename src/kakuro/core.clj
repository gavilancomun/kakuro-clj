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

(def v (->Value #{1 2 3 4 5 6 7 8 9}))
(def e (->Empty))
(defn d [n] (->Down n))
(defn a [n] (->Across n))
(defn da [d a] (->DownAcross d a))

(def grid1 [
            [e (d 4) (d 22) e (d 16) (d 3)]
            [(a 3) v v (da 16 6) v v ]
            [(a 18) v v v v v]
            [e (da 17 23) v v v (d 14)]
            [(a 9) v v (a 6) v v]
            [(a 15) v v (a 2) v v]
           ])

(defn draw-row [row] (str (apply str (map draw row)) "\n"))

(defn draw-grid [grid] (apply str (map draw-row grid)))

(defn row-across-sums [row]
  (->> (range 0 (count row))
       (filter #(:across (get row %)))
       (map #(->[(:across (get row %)) (take-while (fn [c] (:values c)) (drop (inc %) row))]))
    ))

(defn create-across-sums [grid] (map row-across-sums grid))
