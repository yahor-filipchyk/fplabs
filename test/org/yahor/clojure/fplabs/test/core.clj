(ns org.yahor.clojure.fplabs.test.core
  (:use [org.yahor.clojure.fplabs.clusterization.estimation])
  (:use [clojure.test])
  (:require [clojure.java.io :as io]))

(deftest split-string
  (testing "String should be splitted by comma and appropriate columns should be extracted"
    (is (= (record "1,1.52101,13.64,4.49,1.10,71.78,0.06,8.75,0.00,0.00,1" (range 1 10))
           [1.52101 13.64 4.49 1.10 71.78 0.06 8.75 0.00 0.00]))))

(deftest get-first-record-from-file
  (testing "First record in file bezdekIris.data.txt"
    (is (= (first (get-data (io/resource "bezdekIris.data.txt") (range 4)))
           [5.1 3.5 1.4 0.2]))))

(deftest get-last-record-from-file
  (testing "Last record in file glass.data.txt"
    (is (= (last (get-data (io/resource "glass.data.txt") (range 1 10)))
           [1.51711 14.23 0.00 2.08 73.36 0.00 8.62 1.67 0.00]))))

(deftest get-records-count
  (testing "Count of records in file glass.data.txt"
    (is (= (count (get-data (io/resource "glass.data.txt") (range 4)))
           214))))

(deftest test-distance
  (testing "Distance between 2 points"
    (is (= (Math/round (* 1000000 (distance [-2.3 4] [8.5 0.7])))
           11292918))))

(deftest check-sub-potential
  (testing "Checking formula e ^ (-a * ||xi - xj|| ^ 2)"
    (is (= (Math/round (* (sub-potential [-2.3 4] [8.5 0.7] 0.5) (Math/pow 10 34)))
           2028674))))

(deftest check-potential
  (def points '([1 3] [-2 4] [0 2]))
  (testing "Checking computing of potential of the 0-th point"
    (is (= (Math/round (* 1000000 (potential points (first points) 0.5)))
           1374617))))

(deftest check-potential-revising
  (def alfa 0.5)
  (def beta (* alfa 1.5))
  (def points '([1 3] [-2 4] [0 2]))
  (def potentials (compute-potentials points alfa))
  (testing "Checking if given potential is revisied correctly"
    (is (= (Math/round (* 1000000 (revise-potential (first potentials) (last potentials) beta)))
           1065315))))