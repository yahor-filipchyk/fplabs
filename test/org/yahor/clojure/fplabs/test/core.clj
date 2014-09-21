(ns org.yahor.clojure.fplabs.test.core
  (:use [org.yahor.clojure.fplabs.clusterization.estimation])
  (:use [clojure.test]))

(deftest estimation-run
  (testing "Cluster estimation run"
    (is (= (run-estimation) nil))))
