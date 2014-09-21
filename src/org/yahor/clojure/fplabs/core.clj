(ns org.yahor.clojure.fplabs.core
  (:gen-class))

(require 'org.yahor.clojure.fplabs.clusterization.estimation)
(refer 'org.yahor.clojure.fplabs.clusterization.estimation)

(defn -main
  [& args]
  (if-not (first args)
    (println "Pass lab number as command line argument")
    (case (first args)
      "1" (run-estimation))))