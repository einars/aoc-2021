(require '[clojure.tools.trace :use all])

(defn towards-0 [n]
  (cond
    (> n 0) (dec n)
    (< n 0) (inc n)
    :else n))

(defn iterate-shoot [[x y] [dx dy] [xmin xmax] [ymin ymax] max-y-reached]
  (cond 
    (and (<= xmin x xmax) (<= ymin y ymax)) max-y-reached
    (and (= dx 0) (< x xmin)) :undershoot
    (and (= dx 0) (> x xmax)) :overshoot
    (and (< dy 0) (< y ymin) (< x xmin)) :undershoot
    (and (< dy 0) (< y ymin) (> x xmax)) :overshoot
    (and (< dy 0) (< y ymin)) :miss
    :else
    (iterate-shoot [(+ x dx) (+ y dy)]
                   [(towards-0 dx) (dec dy)]
                   [xmin xmax]
                   [ymin ymax]
                   (max max-y-reached y))))

(defn take-best [results]
  (let [res (filter (complement keyword?) results)]
    (if (empty? res)
      nil
      (apply max res))))

(defn count-best [results]
  (let [res (filter (complement keyword?) results)]
    (if (empty? res)
      nil
      (apply max res))))

(defn shoot-y [y [adjx adjy] xrange yrange]
  (let [ra (iterate-shoot [0 0] [adjx y] xrange yrange 0)
        rb (iterate-shoot [0 0] [adjx y] xrange yrange 0)]
    (cond 
      (and (= :overshoot ra)
           (= :overshoot rb))
      :enough

      ;(and (= :undershoot ra)
           ;(= :undershoot rb))
      ;:not-enough

      :else
      (take-best (for [x (range adjx adjy)]
        (iterate-shoot [0 0] [x y] xrange yrange y))))))

  



(defn solve [xrange yrange y current-best]
  (let [res (shoot-y y [-100 100] xrange yrange)]
    (do
      (printf "%d %s, cur-best %d\n" y res current-best)
      (condp = res

        :enough
        current-best

        :not-enough
        (recur xrange yrange (inc y) current-best)

        nil
        (recur xrange yrange (inc y) current-best)

        (recur xrange yrange (inc y) (max current-best res))))))

;(prn (solve [20 30] [-10 -5] 0 0))
(prn (solve [70 125] [-159 -121] 0 0))
