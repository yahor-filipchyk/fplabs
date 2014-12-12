(defproject org.yahor.clojure/fplabs "1.0.0-SNAPSHOT"
  :description "Project for Functional programming tasks"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-http "1.0.1"]
                 [org.jsoup/jsoup "1.8.1"]]
  :resource-paths ["resources"]
  :aot :all
  :main org.yahor.clojure.fplabs.core)