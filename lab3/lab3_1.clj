(ns lab3.lab3-1)

(defn heavy [sleep pred]
  (fn [& coll]
    (Thread/sleep sleep)
    (apply pred coll)))

(defn delimiter [coll threads]
  (let [n (Math/round (double (/ (count coll) threads)))]
    (loop [acc [] new_coll coll]
      (if (= (inc (count acc)) threads)
        (conj acc new_coll)
        (recur (conj acc (take n new_coll)) (drop n new_coll))))))

(defn my-filter [coll threads pred]
  (let [pred (heavy 100 pred)]
    (->>
      (delimiter coll threads)
      (map #(future (doall (filter pred %))))
      (doall)
      (map deref)
      (doall)
      (apply concat))))

(time(doall(filter (heavy 100 even?) '(1 2 3 4 5 6))))
(time (my-filter '(1 2 3 4 5 6) 3 even?))