(ns lab3.testlab3-1
  (:use lab3.lab3-1)
  (:require [clojure.test :as test]))

(test/deftest testlab3
  (test/testing "test 3.1 delimeter"
    (test/is (= (delimiter '(1 2 3 4 5 6) 2) '((1 2 3) (4 5 6))))
    (test/is (= (delimiter '(1 2 3 4 5 6) 3) '((1 2) (3 4) (5 6))))
    (test/is (= (delimiter '(1 2 3 4 5 6 7 8 9 10 11) 4) '((1 2 3) (4 5 6) (7 8 9) (10 11))))
    )

  (test/testing "test 3.1 my-filter"
    (test/is (= (my-filter '(1 2 3 4 5 6) 3 even?) '(2 4 6)))
    (test/is (= (my-filter '(1 2 3 4 5 6) 3 odd?) '(1 3 5)))
    (test/is (= (my-filter '(1 2 3 4 5 6) 3 (fn [x] (= x 1))) '(1)))
    )
  )

(test/run-tests 'lab3.testlab3-1)