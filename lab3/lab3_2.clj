(ns lab3.lab3-2)

(defn heavy [sleep pred]
  (fn [& coll]
    (Thread/sleep sleep)
    (apply pred coll)))

(def my-heavy-pred (heavy 100 even?))

(defn block-delimiter [size-block coll]
  (lazy-seq (if (> (count (take size-block coll)) 0)
              (cons (take size-block coll) (block-delimiter size-block (drop size-block coll)))
              ())))

(defn my-filter [pred]
  (fn [coll]
    (->>
      coll
      (map #(future (doall (filter pred %))))
      (doall)
      (mapcat deref)
      (lazy-seq))))

(defn my-filter-ls [mini-block-size block-size pred coll]
  (->>
    (block-delimiter block-size coll)                       ;((1 2 3 4 5) (6 7 8 9 10))
    (map #(block-delimiter mini-block-size %))              ;((1 2 3) (4 5))
    (mapcat #((my-filter pred) %))))

(defn main []
  (let [f (my-filter-ls 2 3 my-heavy-pred (range))]
    (time (println (doall (take 4 f))))))

(main)
