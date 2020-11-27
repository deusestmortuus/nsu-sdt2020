(ns lab3.lab3-2)

(defn heavy [sleep pred]
  (fn [& coll]
    (Thread/sleep sleep)
    (apply pred coll)))

(defn block-delimiter [size-block coll]
  (lazy-seq (if (> (count (take size-block coll)) 0)
      (cons (take size-block coll) (block-delimiter size-block (drop size-block coll)))
      ())))

(defn threads-delimiter [threads coll]
  (let [n (Math/round (double (/ (count coll) threads)))]
    (loop [acc [] new_coll coll]
      (if (= (inc (count acc)) threads)
        (conj acc new_coll)
        (recur (conj acc (take n new_coll)) (drop n new_coll))))))

(defn my-filter [pred]
  (fn [coll]
    (let [pred (heavy 100 pred)]
      (->>
        coll
        (map #(future (doall (filter pred %))))
        (doall)
        (map deref)
        (apply concat)
        (lazy-seq)))))

(defn my-filter-ls [threads block-size pred coll]
  (->>
    (block-delimiter block-size coll) ;((1 2 3 4 5) (6 7 8 9 10))
    (map #(threads-delimiter threads %)) ;((1 2 3) (4 5)) - 300+ 600+
    (map #((my-filter pred) %))
    (apply concat)))

(defn main []
  (let [f (my-filter-ls 2 3 even? (range))]
    (time (println (doall (take 4 f))))
    ))

(main)


