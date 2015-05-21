(ns kakuro.core)

(defprotocol Cell
  (draw [this]))

(defrecord EmptyCell []
  Cell
  (draw [this] "   -----  "))

(defrecord DownCell [down]
  Cell
  (draw [this] (format "   %2d\\--  " (:down this))))

(defrecord AcrossCell [across]
  Cell
  (draw [this] (format "   --\\%2d  " (:across this))))

(defrecord DownAcrossCell [down across]
  Cell
  (draw [this] (format "   %2d\\%2d  " (:down this) (:across this))))

(defrecord ValueCell [values]
  Cell
  (draw [this]
    (let [values (:values this)]
      (if (= 1 (count values))
        (first (map #(str "     " % "    ") values))
        (str " " (apply str (map #(if (contains? values %) % ".") (range 1 10))))))))

