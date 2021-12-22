(require '[clojure.string :as str])
(require '[clojure.tools.trace :use all])

(defn parse-line [s]
  (let [ms (re-matches #"^(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)" s)
        [_ mode xf xt yf yt zf zt] ms]
    {:add (= "on" mode)
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



(defn remove-area [cube area accu]
  (let [[x y z] cube
        [[xa xb] [ya yb] [za zb]] cube
        [[xf xt] [yf yt] [zf zt]] area]
    (cond
      (and (<= xf xa xb xt)
           (<= yf ya yb yt)
           (<= zf za zb zt))
      ; 'cube completely encased in 'area
      accu

      (or (< xb xf) (> xa xt)
          (< yb yf) (> ya yt)
          (< zb zf) (> za zt))
      ; 'cubes don't intersect
      (conj accu cube)

      (< xa xf (inc xb))
      (->> accu
        (remove-area [[xa (dec xf)] y z] area)
        (remove-area [[xf xb] y z] area))

      (< (dec xa) xt xb)
      (->> accu
        (remove-area [[xa xt] y z] area)
        (remove-area [[(inc xt) xb] y z] area))

      (< ya yf (inc yb))
      (->> accu
        (remove-area [x [ya (dec yf)] z] area)
        (remove-area [x [yf yb] z] area))

      (< (dec ya) yt yb)
      (->> accu
        (remove-area [x [ya yt] z] area)
        (remove-area [x [(inc yt) yb] z] area))

      (< za zf (inc zb))
      (->> accu
        (remove-area [x y [za (dec zf)]] area)
        (remove-area [x y [zf zb]] area))

      (< (dec za) zt zb)
      (->> accu
        (remove-area [x y [za zt]] area)
        (remove-area [x y [(inc zt) zb]] area))

      :else
      (assert false))))


(defn apply-b [cubes prob]
  (let [area [(:x prob) (:y prob) (:z prob)]
        flt-cubes (reduce #(remove-area %2 area %1) [] cubes)]
    (if (:add prob)
      (conj flt-cubes area)
      flt-cubes)))

(defn volume [[[xf xt] [yf yt] [zf zt]]]
  (* (inc (- xt xf)) (inc (- yt yf)) (inc (- zt zf))))

(defn solve-b [file]
  (->> file
       (slurp)
       (parse-problem)
       (reduce apply-b [])
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


