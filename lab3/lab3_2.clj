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
    coll
    (block-delimiter mini-block-size)
    (block-delimiter block-size)
    (mapcat #((my-filter pred) %))))

(defn main []
  (let [f (my-filter-ls 2 3 my-heavy-pred (range))]
    (time (println (doall (take 4 f))))))

(main)
