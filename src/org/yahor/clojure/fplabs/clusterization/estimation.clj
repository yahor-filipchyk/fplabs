(ns org.yahor.clojure.fplabs.clusterization.estimation
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn coefficient
  [radius]
  (/ 4 (Math/pow radius 2)))

(defn distance
  [point1 point2]
  (Math/pow (reduce + (into [] (map #(Math/pow (- %1 %2) 2) point1 point2))) 0.5))

(defn sub-potential
  [x1 x2 coeff]
  (Math/pow Math/E (* (- coeff) (Math/pow (distance x1 x2) 2))))

(defn potential
  [points xi alfa]
  (reduce + (for [xj points] (sub-potential xi xj alfa))))

(defn compute-potentials
  [points alfa]
  (map #(vector % (potential points % alfa)) points))

(defn revise-potential
  [Pi Phighest beta]
  (- (nth Pi 1) (* (nth Phighest 1) (sub-potential (first Pi) (first Phighest) beta))))

(defn revise-potentials
  [potentials highest beta]
  (map #(vector (nth % 0) (revise-potential % highest beta)) potentials))

(defn record
  "Slits the string by comma and creates the vector of splitted values"
  [line colsrange]
  (def splitted (str/split line #","))
  (doall (map read-string (into [] (for [i colsrange] (nth splitted i))))))

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
