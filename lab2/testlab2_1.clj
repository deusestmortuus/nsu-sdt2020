(ns lab2.testlab2-1
  (:use lab2.lab2_1)
  (:require [clojure.test :as test]))

(test/deftest testlab2
  (test/testing "test 2.1"
    (test/is (= (integrate exponentiation 100 3) 333482N))
    (test/is (= (integrate exponentiation 28 2) 7336))
    (test/is (= (integrate exponentiation 15 1) 2255/2))
    ))

(test/run-tests 'lab2.testlab2-1)
