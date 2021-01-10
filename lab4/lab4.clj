(ns lab4.lab4)

(defn constant [value]
  {:pre [(boolean? value)]}
  (list ::const value))

(defn constant? [expr]
  (= (first expr) ::const))

(defn constant-value [expr]
  {:pre [(constant? expr)]}
  (second expr))

(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  (= (first expr) ::var))

(defn variable-name [expr]
  {:pre [(variable? expr)]}
  (second expr))

(defn same-variables? [var1 var2]
  (and (variable? var1) (variable? var2) (= (variable-name var1) (variable-name var2))))

; conjunction / AND

(defn conjunction [expr & rest]
  (cons ::and (cons expr rest)))

(defn conjunction? [expr]
  (= (first expr) ::and))

(defn conjunction_values [expr]
  {:pre [(conjunction? expr)]}
  (rest expr))

; disjunction / OR

(defn disjunction [expr & rest]
  (cons ::or (cons expr rest)))

(defn disjunction? [expr]
    (= (first expr) ::or))

(defn disjunction_values [expr]
  {:pre [(disjunction? expr)]}
  (rest expr))

; NOT

(defn invert [expr]
  (cons ::not expr))

(defn invert? [expr]
  (= (first expr) ::not))

(defn invert_value [expr]
  {:pre [(invert? expr)]}
  (rest expr))

; implication

(defn implication [expr_left expr_right]
  (cons ::impl (list expr_left expr_right)))

(defn implication? [expr]
  (= (first expr) ::impl))

(declare calculate)
(defn calculate_conjunction [expr var value]
  (let [res (map #(calculate % var value) (rest expr))]
    (if (every? #(constant? %) res)
      (constant (every? true? (map constant-value res)))
      (if (some #(and (constant? %) (not (constant-value %))) res)
        (constant false)
        (apply conjunction res)))))

(defn calculate_disjunction [expr var value]
  (let [res (map #(calculate % var value) (rest expr))]
    (if (every? #(constant? %) res)
      (constant (boolean (some true? (map constant-value res))))
      (if (some #(and (constant? %) (constant-value %)) res)
        (constant true)
        (apply disjunction res)))
    ))

(defn calculate_invert [expr var value]
  (let [res (calculate (rest expr) var value)]
    (if (constant? res)
      (constant (not (constant-value res)))
      (invert res))))

(defn calculate_implication [expr var value]
  (let [res (map #(calculate % var value) (rest expr))]
    (if (every? #(constant? %) res)
      (if (and (true? (constant-value (first res))) (false? (constant-value (second res))))
        (constant false)
        (constant true))
      (implication (first res) (second res)))))

(def calculate_rules
  (list
    [(fn [expr _ _] (constant? expr)) (fn [expr _ _] expr)]
    [(fn [expr var _] (same-variables? expr var)) (fn [_ _ value] value)]
    [(fn [expr _ _] (variable? expr)) (fn [expr _ _] expr)]
    [(fn [expr _ _] (invert? expr)) calculate_invert]
    [(fn [expr _ _] (disjunction? expr)) calculate_disjunction]
    [(fn [expr _ _] (conjunction? expr)) calculate_conjunction]
    [(fn [expr _ _] (implication? expr)) calculate_implication]
    ))

(defn calculate [expr var value]
  ((some
     (fn [rule]
       (if ((first rule) expr var value)
         (second rule)
         false))
     calculate_rules)
   expr var value))

; DNF
;1) Избавиться от всех логических операций, содержащихся
; в формуле, заменив их основными: конъюнкцией, дизъюнкцией, отрицанием.
; Это можно сделать, используя равносильные формулы

(declare simplification)
(def simplification_rules
  (list
    [(fn [expr] (constant? expr)) (fn [expr] expr)]
    [(fn [expr] (variable? expr)) (fn [expr] expr)]
    [(fn [expr] (invert? expr)) (fn [expr] (invert (simplification (rest expr))))]
    [(fn [expr] (disjunction? expr)) (fn [expr] (apply disjunction (map #(simplification %) (rest expr))))] ;or
    [(fn [expr] (conjunction? expr)) (fn [expr] (apply conjunction (map #(simplification %) (rest expr))))] ;and
    [(fn [expr] (implication? expr)) (fn [expr] (disjunction (invert (simplification (first (rest expr)))) (simplification (second (rest expr)))))]))

(defn simplification [expr]
  ((some (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
         simplification_rules)
   expr))


;2) Заменить знак отрицания, относящийся ко всему выражению,
; знаками отрицания, относящимися к отдельным переменным высказываниям
; на основании формул
;3) Избавиться от знаков двойного отрицания

(defn element_wise_invert [expr]
  (if (invert? expr)
    (let [nextop (rest expr)]
      (cond
        (invert? nextop) (element_wise_invert (invert_value nextop))
        (disjunction? nextop) (apply conjunction (map #(element_wise_invert (invert %)) (disjunction_values nextop)))
        (conjunction? nextop) (apply disjunction (map #(element_wise_invert (invert %)) (conjunction_values nextop)))
        :else (invert nextop)))
    (if (variable? expr)
      expr
      (cons (first expr) (map #(element_wise_invert %) (rest expr))))))

;4) Применить к операциям конъюнкции и дизъюнкции свойства дистрибутивности
(defn glue [expr1 expr2]
  (if (disjunction? expr1)
    (if (disjunction? expr2)
      (for [x (rest expr1) y (rest expr2)] (conjunction x y))
      (for [x (rest expr1)] (conjunction x expr2)))
    (if (disjunction? expr2)
      (for [x (rest expr2)] (conjunction expr1 x))
      (conjunction expr1 expr2))))

(defn distribution_law [expr]
  (cond
    (disjunction? expr) (apply disjunction (map distribution_law (rest expr)))
    (conjunction? expr)
    (let [args (map distribution_law (rest expr))]
      (reduce #(apply disjunction (glue %1 %2)) args))
    :else expr))

(defn simple [expr]
  (cond
    (disjunction? expr)
    (apply disjunction (reduce #(if (disjunction? %2) (concat %1 (rest %2)) (cons %2 %1)) (cons '() (distinct (map simple (rest expr))))))

    (conjunction? expr)
    (apply conjunction (reduce #(if (conjunction? %2) (concat %1 (rest %2)) (cons %2 %1)) (cons '() (distinct (map simple (rest expr))))))
    :else expr))

(defn dnf [expr]
  (simple (distribution_law (simple (element_wise_invert (simplification expr))))))

;(println "Изначальное выражение:   " (invert (disjunction (implication (variable :x) (variable :y)) (invert (implication (variable :y) (variable :z))))))
;(println)
;(println "Должно получиться: " (invert (disjunction (disjunction (invert (variable :x)) (variable :y)) (invert (disjunction (invert (variable :y)) (variable :z))))))
;(println)
;(println "Получилось:   " (simplification (invert (disjunction (implication (variable :x) (variable :y)) (invert (implication (variable :y) (variable :z)))))))
;(println (= (simplification (invert (disjunction (implication (variable :x) (variable :y)) (invert (implication (variable :y) (variable :z)))))) (invert (disjunction (disjunction (invert (variable :x)) (variable :y)) (invert (disjunction (invert (variable :y)) (variable :z)))))))
;
;(println "Получилось   " (element_wise_invert (simplification (invert (disjunction (implication (variable :x) (variable :y)) (invert (implication (variable :y) (variable :z))))))))
;(println "Должно пол   " (conjunction (conjunction (variable :x) (invert (variable :y))) (disjunction (invert (variable :y)) (variable :z))))
;(println (= (element_wise_invert (simplification (invert (disjunction (implication (variable :x) (variable :y)) (invert (implication (variable :y) (variable :z))))))) (conjunction (conjunction (variable :x) (invert (variable :y))) (disjunction (invert (variable :y)) (variable :z)))))
;
;(println "Получилось   " (simple (distribution_law (simple (element_wise_invert (simplification (invert (disjunction (implication (variable :x) (variable :y)) (invert (implication (variable :y) (variable :z)))))))))))
;(println "Должно пол   " (disjunction (conjunction (variable :x) (invert (variable :y))) (conjunction (variable :x) (invert (variable :y)) (variable :z))))
;(println (= (simple (distribution_law (simple (element_wise_invert (simplification (invert (disjunction (implication (variable :x) (variable :y)) (invert (implication (variable :y) (variable :z)))))))))) (disjunction (conjunction (variable :x) (invert (variable :y))) (conjunction (variable :x) (invert (variable :y)) (variable :z)))))
