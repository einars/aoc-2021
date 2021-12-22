(require '[clojure.string :as str])

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




(defn solve-a [file]
  (->> file
       (slurp)
       (parse-problem)
       (reduce apply-a #{})
       (count)))

(assert (= 590784 (solve-a "data/test-22.txt")))
(prn (solve-a "data/input-22.txt"))


