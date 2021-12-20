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
  (into {} (map (fn [v idx] [(int->bin9 idx) (if (= \# v) 1 0)]) s (range))))

(defn make-coordinate-map [[line y]]
  (let [line-and-x (map list line (range))]
    (reduce (fn [accu [elem x]] (assoc accu [x y] (if (= \# elem) 1 0)))
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
    (rules idx)))

(defn apply-rules [[image rules inf]]
  (let [min-range (apply min (map first (keys image))) ; assume square
        max-range (apply max (map first (keys image)))
        proc-range (range (- min-range 1) (inc (+ max-range 1)))
        all-coords (for [x proc-range, y proc-range] [x y])
        new-image (zipmap all-coords
                          (map #(apply-rules-at % image rules inf) all-coords))
        new-inf (rules [inf inf inf inf inf inf inf inf inf])]
    [new-image rules new-inf]))

(defn get-result-a [image]
  (count (filter (partial = 1) (vals image))))

(defn solve-problem [file n-iterations]
  (let [[rules image] (parse-problem (slurp file))]
    (get-result-a (first (nth (iterate apply-rules [image rules 0]) n-iterations)))))

; ---

(assert (= 35 (solve-problem "data/test-20.txt" 2)))
(assert (= 3351 (solve-problem "data/test-20.txt" 50)))

(prn (solve-problem "data/input-20.txt" 2))
(prn (solve-problem "data/input-20.txt" 50))
