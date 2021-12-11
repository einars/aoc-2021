(def all-dominoes
  (set (for [a (range 0 7)
             b (range a 7)]
         (list a b))))

(defn makes-wheel? [[a1 a2] [b1 _b2]]
  (or (= b1 (+ a1 a2))
      (= a1 (+ a2 b1))
      (= a2 (+ b1 a1))))

(defn find-possible-wheels-3 [ds d1 d2 d3]
  (map #(list d1 d2 d3 %) (filter #(and (makes-wheel? d3 %)
                                        (makes-wheel? % d1))
                                  ds)))

(defn find-possible-wheels-2 [ds d1 d2]
  (loop [items (filter #(makes-wheel? d2 %) ds)
         accu #{}]
    (if (empty? items)
      accu
      (recur (rest items)
             (reduce conj accu (find-possible-wheels-3 (filter (partial not= (first items)) ds)
                                                       d1
                                                       d2
                                                       (first items)))))))

(defn find-possible-wheels-1 [ds d1]
  (loop [items (filter #(makes-wheel? d1 %) ds)
         accu #{}]
    (if (empty? items)
      accu
      (recur (rest items)
             (reduce conj accu (find-possible-wheels-2 (filter (partial not= (first items)) ds)
                                                       d1
                                                       (first items)))))))

(defn find-possible-wheels [ds]
  (loop [items ds
         accu #{}]
    (if (empty? items)
      accu
      (recur (rest items)
             (reduce conj accu (find-possible-wheels-1 (rest items)
                                                       (first items)))))))

(defn consider-success [wheels]
  (when (= 7 (count wheels))
    (prn wheels)))

(defn solve-dominoes [ds wheels-so-far]
  (loop [items (find-possible-wheels ds)]
    (if (empty? items)
      (consider-success wheels-so-far)
      (do
        (when (empty? wheels-so-far) (prn (first items) (count items)))
        (solve-dominoes (reduce disj ds (first items)) (conj wheels-so-far (first items)))
        (recur (rest items))))))

(solve-dominoes all-dominoes [])
