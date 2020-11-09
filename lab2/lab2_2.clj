(ns lab2.lab2_2)

(defn exponentiation [x]
  (* x x))

(defn trap [func x1 x2]
  (* (/ (+ (func x1) (func x2)) 2) (- x2 x1)))

(defn lazy-integral [step func]
  (map first (iterate (fn [[acc x]] [(+ (trap func x (+ x step)) acc) (+ x step)]) [0 0])))

(defn get-integral [step func]
  (fn [x] (nth (lazy-integral step func) (/ x step))))

(let [step 1 integral (get-integral step exponentiation)]
  (println(time (integral 10)))
  (println(time (integral 10)))
  (println(time (integral 5)))
  (println(time (integral 15))))