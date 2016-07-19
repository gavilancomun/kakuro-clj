;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit alt+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns gavilan.ws1
  (:require [gorilla-plot.core :as plot]
            [clojure.spec :as s]
            [kakuro.core :as k]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(s/def ::i integer?)
(s/def ::s string?)
(s/def ::k (s/tuple ::i ::s))

(s/explain ::k [1 2])

;; @@
;; ->
;;; In: [1] val: 2 fails spec: :gavilan.ws1/s at: [1] predicate: string?
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(s/instrument-ns 'kakuro.core)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
