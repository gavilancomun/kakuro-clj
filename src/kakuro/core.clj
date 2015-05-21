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

