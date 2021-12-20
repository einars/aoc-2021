(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.tools.trace :use all])

(defn int->bin9 [n]
  (reverse (for [bit (range 9)] (if (bit-test n bit) 1 0))))

(assert (= (int->bin9 34) [0 0 0 1 0 0 0 1 0]))

(defn neighbors [[x y]]
  [[(dec x) (dec y)]
   [     x  (dec y)]
   [(inc x) (dec y)]
   [(dec x)      y ]
   [     x       y ]
   [(inc x)      y ]
   [(dec x) (inc y)]
   [     x  (inc y)]
   [(inc x) (inc y)]])

(defn parse-rules [s]
  (into {} (map (fn [v idx] [ (int->bin9 idx) (if (= \# v) 1 0) ] ) s (range))))

(defn make-coordinate-map [[line y]]
  (let [line-and-x (map list line (range))]
    (reduce (fn [accu [elem x]] (assoc accu (list x y) (if (= \# elem) 1 0)))
            {}
            line-and-x)))

(defn parse-image [s]
  (reduce merge (map make-coordinate-map (map list (str/split s #"\n") (range)))))

(defn parse-problem [p]
  (let [[rules-s image-s] (str/split p #"\n\n")
        rules (parse-rules rules-s)
        image (parse-image image-s)]
    [rules image]))

(defn apply-rules-at [coord image rules inf]
  (let [idx (map #(image % inf) (neighbors coord))]
    (rules idx nil)))

(defn print-image [image]
  (let [min-range (apply min (map first (keys image))) ; assume square
        max-range (apply max (map first (keys image)))
        proc-range (range (- min-range 2) (inc (+ max-range 2)))
        pixels (into {} [[nil "~"] [0 "."] [1 "#"]])]
    (count (for [y proc-range, x proc-range]
             (do
               (if (= x (- min-range 2)) (printf "\n"))
               (printf "%s" (pixels (image [x y]))))))
    (println)
    image))

(defn apply-rules [image rules inf]
  (let [min-range (apply min (map first (keys image))) ; assume square
        max-range (apply max (map first (keys image)))
        proc-range (range (- min-range 2) (inc (+ max-range 2)))
        new-image (reduce #(assoc %1 (first %2) (second %2)) 
                          {}
                          (for [x proc-range, y proc-range]
                            (list (list x y)
                                  (apply-rules-at [x y] image rules inf))))]
    [new-image (rules [inf inf inf inf inf inf inf inf inf])]))

(defn iterate-problem [image rules inf n-iterations]
  (if (= n-iterations 0) 
    image
    (let [[new-image new-inf] (apply-rules image rules inf)]
      (recur new-image rules new-inf (dec n-iterations)))))

(defn get-result-a [image]
  (count (filter (partial = 1) (vals image))))

(defn solve-problem [file n-iterations]
  (let [ [rules image] (parse-problem (slurp file))]
    (get-result-a (iterate-problem image rules 0 n-iterations))))

; ---

(assert (= 35 (solve-problem "data/test-20.txt" 2)))
(assert (= 3351 (solve-problem "data/test-20.txt" 50)))
(prn (solve-problem "data/input-20.txt" 2))
(prn (solve-problem "data/input-20.txt" 50))
