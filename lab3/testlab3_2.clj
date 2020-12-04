(ns lab3.testlab3-2
  (:use lab3.lab3-2)
  (:require [clojure.test :as test]))

(test/deftest testlab3
  (test/testing "test 3.2 threads delimeter"
    (test/is (= (threads-delimiter 2 '(1 2 3 4 5 6)) '((1 2 3) (4 5 6))))
    (test/is (= (threads-delimiter 3 '(1 2 3 4 5 6)) '((1 2) (3 4) (5 6))))
    (test/is (= (threads-delimiter 4 '(1 2 3 4 5 6 7 8 9 10 11)) '((1 2 3) (4 5 6) (7 8 9) (10 11))))
    )

  (test/testing "test 3.2 block delimeter"
    (test/is (= (block-delimiter 2 '(1 2 3 4 5 6)) '((1 2) (3 4) (5 6))))
    (test/is (= (block-delimiter 4 '(1 2 3 4 5 6)) '((1 2 3 4) (5 6))))
    (test/is (= (block-delimiter 4 '(1 2 3 4 5 6 7 8 9 10 11)) '((1 2 3 4) (5 6 7 8) (9 10 11))))
    )

  (test/testing "test 3.2 my-filter"
    (test/is (= (take 3 (my-filter-ls 2 3 even? (range))) '(0 2 4)))
    )
  )

(test/run-tests 'lab3.testlab3-2)