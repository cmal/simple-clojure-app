(ns simple-clojure-app.geeksforgeeks
  (:require
   [clojure.string :as str]
   [clojure.repl :refer [source doc]])
  (:use clojure.test))

;; https://www.geeksforgeeks.org/program-find-covariance/

(defn mean [v]
  (/ (reduce + v) (count v)))

(def a1 [65.21M 64.75M 65.26M 65.76M 65.96M])
(def a2 [67.25M 66.39M 66.12M 65.70M 66.64M])
(def a3 [5M 20M 40M 80M 100M])
(def a4 [10M 24M 33M 54M 10M])

(defn cov [xs ys]
  (let [xm (mean xs)
        ym (mean ys)
        cnt (dec (count xs))
        sum (reduce + (map (fn [x y] (* (- x xm) (- y ym))) xs ys))]
    (/ sum cnt)))

(deftest mean-test
  (is (= 2 (mean [2 2])))
  (is (= 2 (mean [1 3]))))

(deftest cov-test
  (is (= -0.05805M (cov a1 a2)))
  (is (= 187.75M (cov a3 a4))))


