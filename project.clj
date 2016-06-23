(defproject kakuro-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.9.0-alpha7"]
                 [org.clojure/core.logic "0.8.10"]
                 [org.clojure/test.check "0.9.0"]
                 [pjstadig/humane-test-output "0.8.0"]]
  :plugins [[lein-gorilla "0.3.6"]]
  :injections [(require 'pjstadig.humane-test-output)
               (pjstadig.humane-test-output/activate!)])
