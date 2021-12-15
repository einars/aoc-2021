(require '[clojure.string :as str])

(use 'clojure.tools.trace)

(defn make-coordinate-map [[line y]]
  (let [line-and-x (map list line (range))]
    (reduce (fn [accu [elem x]] (assoc accu (list x y) (Integer/parseInt (str elem)) ))
            {}
            line-and-x)))

(defn read-xy-map [file]
  (reduce merge (map make-coordinate-map (map list (str/split (slurp file) #"\n") (range)))))

(defn neighbors [[x y]]
  (list [(inc x) y]
        [x (inc y)]
        [(dec x) y]
        [x (dec y)]))

(defn improve-around-neighbors [m best-paths accu pt]
  (->> (neighbors pt)
       (filter m) ; neighbors on map
       (map #(list % (+ (m %) (best-paths pt)))) ; neighbor list of [point score]
       (filter (fn [[pt score]]
                 (and (or (nil? (best-paths pt)) ; leave only new or improved
                          (< score (best-paths pt)))
                      (or (nil? (accu pt))
                          (< score (accu pt))))))
       (reduce #(assoc %1 (first %2) (second %2)) accu)))

(defn trace-path
  ([m] (trace-path m (assoc {} [0 0] 0) [[0 0]] ))

  ([m best-paths frontier]
   (let [new-frontier (reduce (partial improve-around-neighbors m best-paths)
                              {}
                              frontier)]
     (do (printf ".")
         (flush)
         (if (empty? new-frontier)
           best-paths
           (recur m
                  (merge best-paths new-frontier)
                  (keys new-frontier)))))))

(defn bottom-right [area] 
  (list (apply max (map first (keys area)))
        (apply max (map second (keys area)))))

(defn at-bottom-right [area] (area (bottom-right area)))

(defn clamp-risk [r]
  (if (> r 9) (- r 9) r))

(defn build-huge-map [m]
  (let [[sx sy] (bottom-right m)
        all-keys (keys m)
        extra-squares (for [rx (range 0 5)
                            ry (range 0 5)]
                        (map (fn [[x y]] (list (list (+ x (* (inc sx) rx))
                                                     (+ y (* (inc sy) ry)))
                                               (clamp-risk (+ (m [x y]) rx ry))))
                             all-keys))]
    (reduce #(apply assoc %1 %2)
            {}
            (reduce concat extra-squares))))

(defn print-map [m]
  (let [[sx sy] (bottom-right m)]
    (count (for [y (range (inc sy))
                 x (range (inc sx))]
             (do
               (when (= x 0)
                 (printf "\n"))
               (printf "%d" (m [x y] "-")))))))

; ---

(assert (= 40 (at-bottom-right (trace-path (read-xy-map "data/test-15.txt")))))
(assert (= 315 (at-bottom-right (trace-path (build-huge-map (read-xy-map "data/test-15.txt"))))))

(prn (at-bottom-right (trace-path (read-xy-map "data/input-15.txt"))))
(prn (at-bottom-right (trace-path (build-huge-map (read-xy-map "data/input-15.txt")))))

;(print-map (build-huge-map (read-xy-map "data/test-15.txt")))
