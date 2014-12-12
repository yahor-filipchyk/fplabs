(ns org.yahor.clojure.fplabs.clusterization.estimation
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def eps-upper 0.5)
(def eps-lower 0.15)

(defn coefficient
  [radius]
  (/ 4 (Math/pow radius 2)))

(defn euclidean-distance
  [point1 point2]
  (Math/pow (reduce + (into [] (map #(Math/pow (- %1 %2) 2) point1 point2))) 0.5))

(defn hamming-distance
  [point1 point2]
  (reduce + (into [] (map #(Math/abs (- %1 %2)) point1 point2))))

(defn sub-potential
  [x1 x2 coeff distance]
  (Math/pow Math/E (* (- coeff) (Math/pow (distance x1 x2) 2))))

(defn potential
  [points xi alfa distance]
  (reduce + (for [xj points] (sub-potential xi (first xj) alfa distance))))

(defn compute-potentials
  [points alfa distance]
  (doall (map #(vector % (potential points (first %) alfa distance)) points)))

(defn revise-potential
  "Revise a single potential and returns revised potential"
  [Pi Phighest beta distance]
  ;(println Pi Phighest)
  (-
    (nth Pi 1)
    (* (peek Phighest) (sub-potential (first (first Pi)) (first (first Phighest)) beta distance))))

(defn revise-potentials
  "Returns vector of revised potentials"
  [potentials highest beta distance]
  (doall (map #(vector (first %) (revise-potential % highest beta distance)) potentials)))

(defn max-potential
  [potentials]
  ;(println potentials)
  ;(reduce
  ;  (fn [p1 p2]
  ;    (if (> (peek p1) (peek p2))
  ;      p1
  ;      p2)) potentials))
  (let [size (- (count potentials) 1)]
    (loop [i 1
           pos 0
           points (rest potentials)
           max (first potentials)]
      (if (>= i size)
        [max pos]
        (if (> (last (first points)) (last max))
          (recur (inc i) i (rest points) (first points))
          (recur (inc i) pos (rest points) max))))))

(defn d-min
  [Pk centers distance]
  (reduce min (doall (map #(distance (first (first Pk)) (first (first %))) centers))))

(defn do-estimation
  [Pk-with-pos P1 potentials rad-a already-found distance]
  (let [Pk (first Pk-with-pos)
        Pk-potential (peek Pk)
        P1-potential (peek P1)
        coeff-b (coefficient (* 1.5 rad-a))]
  ;(println "Pk " Pk " rad-a " rad-a " found: " already-found)
    (if (> Pk-potential (* eps-upper P1-potential))
      (recur
         (max-potential (revise-potentials potentials Pk coeff-b distance))
         P1
         (revise-potentials potentials Pk coeff-b distance)
         rad-a
         (conj already-found Pk)
         distance)
      (if (< Pk-potential (* eps-lower P1-potential))
        already-found ; exiting
        (if (>= (+ (/ (d-min Pk already-found distance) rad-a) (/ Pk-potential P1-potential)) 1)
          (recur
             (max-potential (revise-potentials potentials Pk coeff-b distance))
             P1
             (revise-potentials potentials Pk coeff-b distance)
             rad-a
             (conj already-found Pk)
             distance)
          (recur
             (max-potential (revise-potentials potentials Pk coeff-b distance))
             P1
             (revise-potentials potentials Pk coeff-b distance)
             rad-a
             already-found
             distance))))))

(defn estimate
  [points rad-a distance]
  ;(println points)
  (let [coeff-b (coefficient (* 1.5 rad-a))
        potentials (compute-potentials points (coefficient rad-a) distance)
        highest (first (max-potential potentials))
        revised (revise-potentials potentials highest coeff-b distance)]
        (do-estimation
          (max-potential revised)
          highest
          revised
          rad-a
          [highest]
          distance)
        ))

(defn record
  "Slits the string by comma and creates the vector of splitted values"
  [line colsrange label-col]
  (let [splitted (str/split line #",")]
    [(doall (map read-string (into [] (for [i colsrange] (nth splitted i)))))
     (nth splitted label-col)]))

(defn get-data
  "Reads data from file and returns list of records with their characteristics"
  [furl colsrange label-col]
  (with-open [file (io/reader furl)]
    (let [lines (filter #(not (str/blank? %)) (line-seq file))]
      (doall (map #(record % colsrange label-col) lines)))))

(defn print-result
  [filename centers]
  (println filename)
  (doall (map #(println (peek (first %)) (first (first %)) (peek %)) centers)))

(defn run-estimation
  [distance-algorithm]
  (let [distance (if (= "e" distance-algorithm) euclidean-distance hamming-distance)]
  (println "Cluster estimation was started...")
  (if-not (io/resource "bezdekIris.data.txt")
    (println "File 'bezdekIris.data.txt' wasn't found")
    (print-result
      "bezdekIris.data.txt"
      (estimate (get-data (io/resource "bezdekIris.data.txt") (range 4) 4) 1.5 distance)))
                                                                          ; ^  rad-a
                                                                          ; |
  (if-not (io/resource "glass.data.txt")
    (println "File 'glass.data.txt' wasn't found")
    (print-result
      "glass.data.txt"              ;1 2 3 5 6 7 - claster labels for glass.data.txt
      (estimate (get-data (io/resource "glass.data.txt") (range 1 10) 10) 0.76 distance)))))
                                                                         ; ^  rad-a
                                                                         ; |