(ns lab2.lab2_1)

(defn exponentiation [x]
  (* x x))

(defn trap [func x1 x2]
  (* (/ (+ (func x1) (func x2)) 2) (- x2 x1)))

(def memo-solve
  (memoize
    (fn [func end step]
      (if (<= end 0)
        0
        (+ (memo-solve func (- end step) step) (trap func (- end step) end))))))

(defn integrate [func end step]
  (if (= 0 (mod end step))
    (memo-solve func end step)
    (+ (trap func (- end (mod end step)) end) (memo-solve func (- end (mod end step)) step))))

(println (integrate exponentiation 15 1))
(println "memo:")
(time (integrate exponentiation 100 3))
(time (integrate exponentiation 100 3))
(time (integrate exponentiation 99 3))