(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.tools.trace :use all])

(def rotators
  [
   ; facing positive z
   (fn [[x y z]] [   x     y     z ])
   (fn [[x y z]] [(- y)    x     z ])
   (fn [[x y z]] [(- x) (- y)    z ])
   (fn [[x y z]] [   y  (- x)    z ])
   ; facing negative z
   (fn [[x y z]] [   x  (- y) (- z)])
   (fn [[x y z]] [(- y) (- x) (- z)])
   (fn [[x y z]] [(- x)    y  (- z)])
   (fn [[x y z]] [   y     x  (- z)])
                                   
   ; facing positive y
   (fn [[x y z]] [   x     z  (- y)])
   (fn [[x y z]] [(- y)    z  (- x)])
   (fn [[x y z]] [(- x)    z     y ])
   (fn [[x y z]] [   y     z     x ])
                                   
   ; facing negative y
   (fn [[x y z]] [   x  (- z)    y ])
   (fn [[x y z]] [(- y) (- z)    x ])
   (fn [[x y z]] [(- x) (- z) (- y)])
   (fn [[x y z]] [   y  (- z) (- x)])
                                   
   ; facing positive x
   (fn [[x y z]] [   z     y  (- x)])
   (fn [[x y z]] [   z     x     y ])
   (fn [[x y z]] [   z  (- y)    x ])
   (fn [[x y z]] [   z  (- x) (- y)])
                                   
   ; facing negative x
   (fn [[x y z]] [(- z)    y     x ])
   (fn [[x y z]] [(- z)    x  (- y)])
   (fn [[x y z]] [(- z) (- y) (- x)])
   (fn [[x y z]] [(- z) (- x)    y ])
   ])


(defn parse-coords [s]
  (let [cs (str/split s #",")]
    (map #(Integer/parseInt %) cs)))

(defn parse-scanner [s]
  (let [lines (str/split s #"\n")
        name-parts (str/split (first lines) #" ")
        name (nth name-parts 2)
        coords (rest lines)]
    (map parse-coords coords)))

(defn find-match [r0 r]
  (let [set-all (set r0)
        rotations (map #(map % r) rotators)]
    (loop [rots rotations
          match-a r0
          match-b (drop 11 (first rots))]
      (cond
        (empty? rots)
        nil

        ; one full rotation dot-to-dot checked
        (empty? match-a)
        (recur (rest rots) r0 (drop 11 (first (rest rots))))

        ; 
        (empty? match-b)
        (recur rots (rest match-a) (drop 11 (first rots)))

        :else
        (let [rotated (first rots)
              delta (map - (first match-a) (first match-b))
              target (map #(map + % delta) rotated)
              n-intersections (count (set/intersection set-all (set target)))]
          (do (assert (> n-intersections 0))
              (if (>= n-intersections 12)
                (do (prn "found match" (count set-all))
                    (vec (set/union set-all (set target))))
                (recur rots match-a (rest match-b)))))))))



(defn parse-file [input]
  (map parse-scanner (str/split input #"\n\n")))

(defn solve 
  ([[s0 & sxs]] (solve s0 sxs))
  ([s0 sxs]
    (if (empty? sxs)
      (count s0)
      (loop [scanners sxs]
        (if (empty? scanners)
          (do
            (prn scanners)
            (assert false "unable to find match"))
          (let [beacons (find-match s0 (first scanners))]
            (if (nil? beacons)
              (recur (rest scanners))
              (solve beacons (filter #(not= % (first scanners)) sxs)))))))))


;(prn (solve (parse-file (slurp "data/test-19.txt"))))
(prn (solve (parse-file (slurp "data/input-19.txt"))))
