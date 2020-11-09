(ns lab2.testlab2-2
  (:use lab2.lab2_2)
  (:require [clojure.test :as test]))

(test/deftest testlab2
  (test/testing "test 2.2"
    (test/is (= ((get-integral 1 exponentiation) 15) 2255/2))
    (test/is (= ((get-integral 4 exponentiation) 100) 333600))
    (test/is (= ((get-integral 5 exponentiation) 25) 10625/2))
    ))

(test/run-tests 'lab2.testlab2-2)