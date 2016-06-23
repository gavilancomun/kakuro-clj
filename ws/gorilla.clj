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
            [gorilla-repl.html :as gh]
            [clojure.spec :as s]
            [kakuro.core :as k]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(gh/html-view "<strong>bold</strong>")
;; @@
;; =>
;;; {"type":"html","content":"<strong>bold</strong>","value":"#gorilla_repl.html.HtmlView{:content \"<strong>bold</strong>\"}"}
;; <=

;; @@

;; @@
