(require '[clojure.string :as str])
(require '[clojure.tools.trace :use all])

(defn parse-line [s]
  (let [ms (re-matches #"^(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)" s)
        [_ mode xf xt yf yt zf zt] ms]
    {:add (if (= "on" mode) true false)
     :x [(Integer/parseInt xf) (Integer/parseInt xt)]
     :y [(Integer/parseInt yf) (Integer/parseInt yt)]
     :z [(Integer/parseInt zf) (Integer/parseInt zt)]
     }))

(defn parse-problem [s]
  (map parse-line (str/split s #"\n")))

(defn apply-a [space prob]
  (let [s (atom space)]
    (doseq [x (range (max -50 (first (:x prob)))
                     (inc (min 50 (second (:x prob)))))
            y (range (max -50 (first (:y prob)))
                     (inc (min 50 (second (:y prob)))))
            z (range (max -50 (first (:z prob)))
                     (inc (min 50 (second (:z prob)))))]
      (swap! s (fn [space]
                 (if (:add prob)
                   (conj space [x y z])
                   (disj space [x y z])))))
    @s))



(defn split-cube [cube against]
  (let [[x y z] cube
        [[xa xb] [ya yb] [za zb]] cube
        [[xf xt] [yf yt] [zf zt]] against]
    (cond
      (and (<= xf xa xb xt)
           (<= yf ya yb yt)
           (<= zf za zb zt))
      ; 'cube completely encased in 'against
      []

      (or (< xb xf) (> xa xt)
          (< yb yf) (> ya yt)
          (< zb zf) (> za zt))
      ; 'cubes don't intersect
      [cube]

      (< xa xf (inc xb))
      (concat
        (split-cube [[xa (dec xf)] y z] against)
        (split-cube [[xf xb] y z] against))

      (< (dec xa) xt xb)
      (concat
        (split-cube [[xa xt] y z] against)
        (split-cube [[(inc xt) xb] y z] against))

      (< ya yf (inc yb))
      (concat
        (split-cube [x [ya (dec yf)] z] against)
        (split-cube [x [yf yb] z] against))

      (< (dec ya) yt yb)
      (concat
        (split-cube [x [ya yt] z] against)
        (split-cube [x [(inc yt) yb] z] against))

      (< za zf (inc zb))
      (concat
        (split-cube [x y [za (dec zf)]] against)
        (split-cube [x y [zf zb]] against))

      (< (dec za) zt zb)
      (concat
        (split-cube [x y [za zt]] against)
        (split-cube [x y [(inc zt) zb]] against))

      :else
      (assert false))))


(defn apply-b [cubes prob]
  (prn prob (count cubes))
  (let [target [ (:x prob) (:y prob) (:z prob)]
        flt-cubes (reduce concat (map (fn [c] (split-cube c target)) cubes))]
    flush()
    (if (:add prob)
      (conj flt-cubes target)
      flt-cubes)))

(defn volume [[[xf xt] [yf yt] [zf zt]]]
  (* (inc (- xt xf)) (inc (- yt yf)) (inc (- zt zf))))

(defn debug-count [n]
  (prn (count n))
  n)


(defn solve-b [file]
  (->> file
       (slurp)
       (parse-problem)
       (reduce apply-b [])
       (debug-count)
       (map volume)
       (apply +)))

(defn solve-a [file]
  (->> file
       (slurp)
       (parse-problem)
       (reduce apply-a #{})
       (count)))

;(assert (= 590784 (solve-a "data/test-22.txt")))
;(prn (solve-a "data/input-22.txt"))

;(prn (split-cube [[-5 5] [-5 5] [-5 5]] [[-1 1] [-1 1] [-1 1]]))
(prn (volume [[-1 1] [-1 1] [-1 1]]))


(assert (= 2758514936282235 (solve-b "data/test-22b.txt")))
(prn (solve-b "data/input-22.txt"))


