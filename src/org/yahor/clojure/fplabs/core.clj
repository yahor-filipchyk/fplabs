(ns org.yahor.clojure.fplabs.core
  (:gen-class)
  (:require [org.yahor.clojure.fplabs.clusterization.estimation :as estimation]
            [org.yahor.clojure.fplabs.crawling.crawler :as crawler]))

;(require 'org.yahor.clojure.fplabs.clusterization.estimation)
;(refer 'org.yahor.clojure.fplabs.clusterization.estimation)

(defn -main
  [& args]
  (if-not (first args)
    (println "Pass lab number as command line argument")
    (case (first args)
      "1" (estimation/run-estimation (first (rest args)))
      "2" (crawler/crawl (rest args)))))