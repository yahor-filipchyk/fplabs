(ns org.yahor.clojure.fplabs.test.core
  (:use [org.yahor.clojure.fplabs.clusterization.estimation])
  (:use [clojure.test])
  (:require [clojure.java.io :as io]))

(deftest split-string
  (testing "String should be splitted by comma and appropriate columns should be extracted"
    (is (= (record "1,1.52101,13.64,4.49,1.10,71.78,0.06,8.75,0.00,0.00,1" (range 1 10))
           '(1.52101 13.64 4.49 1.10 71.78 0.06 8.75 0.00 0.00)))))

(deftest get-first-record-from-file
  (testing "First record in file bezdekIris.data.txt"
    (is (= (first (get-data (io/resource "bezdekIris.data.txt") (range 4)))
           '(5.1 3.5 1.4 0.2)))))

(deftest get-last-record-from-file
  (testing "Last record in file glass.data.txt"
    (is (= (last (get-data (io/resource "glass.data.txt") (range 1 10)))
           '(1.51711 14.23 0.00 2.08 73.36 0.00 8.62 1.67 0.00)))))

(deftest get-records-count
  (testing "Count of records in file glass.data.txt"
    (is (= (count (get-data (io/resource "glass.data.txt") (range 4)))
           214))))
