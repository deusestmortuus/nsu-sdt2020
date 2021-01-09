(ns lab4.testlab4
  (:use lab4.lab4)
  (:require [clojure.test :as test]))

(test/deftest testlab4
  (test/testing

    (test/is (constant-value (calculate (variable :y) (variable :y) (constant true))))

    (test/is (not (constant-value (calculate (invert (variable :y)) (variable :y) (constant true)))))

    (test/is (constant-value (calculate (disjunction (invert (variable :y)) (variable :y)) (variable :y) (constant true))))

    (test/is (constant-value (calculate (disjunction (variable :y) (variable :y)) (variable :y) (constant true))))

    (test/is (constant-value (calculate (conjunction (variable :y) (variable :y)) (variable :y) (constant true))))

    (test/is (constant-value (calculate (conjunction (invert (variable :y)) (invert (variable :y))) (variable :y) (constant false))))

    (test/is (constant-value (calculate (conjunction (invert (invert (variable :y))) (variable :y)) (variable :y) (constant true))))

    (test/is (constant-value (calculate (implication (variable :y) (constant true)) (variable :y) (constant true))))

    (test/is (constant-value (calculate (implication (variable :y) (constant true)) (variable :y) (constant false))))

    (test/is (not (constant-value (calculate (implication (variable :y) (constant false)) (variable :y) (constant true)))))

    (test/is (constant-value (calculate (disjunction (conjunction (variable :x) (invert (variable :y))) (variable :y)) (variable :y) (constant true))))

    (test/is (= (simplification (invert (disjunction (implication (variable :x) (variable :y)) (invert (implication (variable :y) (variable :z))))))
                (invert (disjunction (disjunction (invert (variable :x)) (variable :y)) (invert (disjunction (invert (variable :y)) (variable :z)))))))

    (test/is (= (element_wise_invert (invert (disjunction (variable :a) (variable :b) (variable :c) (variable :d))))
                (conjunction (invert (variable :a)) (invert (variable :b)) (invert (variable :c)) (invert (variable :d)))))

    (test/is (= (element_wise_invert (invert (conjunction (variable :a) (variable :b) (variable :c) (variable :d))))
                (disjunction (invert (variable :a)) (invert (variable :b)) (invert (variable :c)) (invert (variable :d)))))

    (test/is (= (distribution_law (conjunction (disjunction (variable :z) (invert (variable :y))) (invert (variable :y)) (variable :x)))
                (disjunction (conjunction (conjunction (variable :z) (invert (variable :y))) (variable :x)) (conjunction (conjunction (invert (variable :y)) (invert (variable :y))) (variable :x)))))

    (test/is (= (dnf (conjunction (disjunction (variable :z) (invert (variable :y))) (invert (variable :y)) (variable :x)))
                (disjunction (conjunction (conjunction (variable :z) (invert (variable :y))) (variable :x)) (conjunction (conjunction (invert (variable :y)) (invert (variable :y))) (variable :x)))))
    ))

(test/run-tests 'lab4.testlab4)