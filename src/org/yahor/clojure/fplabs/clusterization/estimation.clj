(ns org.yahor.clojure.fplabs.clusterization.estimation
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn record
  "Slits the string by comma and creates the vector of splitted values"
  [line colsrange]
  (def splitted (str/split line #","))
  (doall (map read-string (for [i colsrange] (nth splitted i)))))

(defn get-data
  "Reads data from file and returns list of records with their characteristics"
  [furl colsrange]
  (with-open [file (io/reader furl)]
    (def lines
      (filter #(not (str/blank? %))
        (line-seq file)))
    (doall (map #(record % colsrange) lines))))

(defn run-estimation
  []
  (println "Cluster estimation was started...")
  (if-not (io/resource "bezdekIris.data.txt")
    (println "File 'bezdekIris.data.txt' wasn't found")
    (get-data (io/resource "bezdekIris.data.txt") (range 4)))
  (if-not (io/resource "glass.data.txt")
    (println "File 'glass.data.txt' wasn't found")
    (get-data (io/resource "glass.data.txt") (range 1 10))))
