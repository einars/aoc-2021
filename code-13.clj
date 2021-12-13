
(require '[clojure.string :as str])


(defn make-point [p]
  (let [[x y] (str/split p #",")]
    (list (Integer/parseInt x) (Integer/parseInt y))))

(defn make-fold [f]
  (let [xy (keyword (str (nth f 11)))
        split (str/split f #"=")
        coord (Integer/parseInt (second split))]
    (list xy coord)))

(defn make-point-set [ps] (set (map make-point ps)))

(defn make-fold-list [ps] (map make-fold ps))


(defn fold-against-x [[x y] fold-x]
  {:pre [ (not= x fold-x) ]}
  (if (<= x fold-x)
    [x y]
    [(- fold-x (- x fold-x)) y]))

(defn fold-against-y [[x y] fold-y]
  {:pre [ (not= y fold-y) ]}
  (if (<= y fold-y)
    [x y]
    [x (- fold-y (- y fold-y))]))

(defn fold-point [pt [xy coord]]
  (if (= xy :x)
    (fold-against-x pt coord)
    (fold-against-y pt coord)))

(defn apply-fold [pts fold]
  (set (map #(fold-point % fold) pts)))

(defn parse-input [file]
  (let [[points folds] (str/split (slurp file) #"\n\n")
        point-set (make-point-set (str/split points #"\n"))
        fold-list (make-fold-list (str/split folds #"\n"))]
    (list point-set fold-list)))

(defn solve-a [file]
  (let [[points folds] (parse-input file)]
    (apply-fold points (first folds))))
    ;(reduce apply-fold points folds)))

(defn solve-b [file]
  (let [[points folds] (parse-input file)
        res (reduce apply-fold points folds)
        max-x (apply max (map first res))
        max-y (apply max (map second res))]
    (count (for [y (range (inc max-y))
                 x (range (inc max-x)) ]
             (do
               (when (= x 0)
                 (printf "\n"))
               (if (res [x y])
                 (printf "X")
                 (printf " ")))))))






(parse-input "data/test-13.txt")
(solve-a "data/test-13.txt")
(assert (= 17 (count (solve-a "data/test-13.txt"))))
(prn (count (solve-a "data/input-13.txt")))
(solve-b "data/input-13.txt")
