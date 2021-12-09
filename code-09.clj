
(defn read-heightmap-line [accu line-and-n]
  (let [line (first line-and-n)
        y (second line-and-n)
        line-and-x (map list line (range))]
    (reduce (fn [tmp-accu [ depth x ]] (assoc tmp-accu (list x y) (Integer/parseInt (str depth)) ))
            accu 
            line-and-x)))


(defn neighbor-coords [c]
  (let [x (first c)
        y (second c)]
    (list [(inc x) y]
          [x (inc y)]
          [(dec x) y]
          [x (dec y)])))

(defn find-low-points [m]
  (filter (fn [coord] 
            (do
              (every?
                (fn [d] (> d (get m coord)))
                (map #(get m % 10) (neighbor-coords coord)))))
          (keys m)))


(defn solve-a [m]
  (let [coords (find-low-points m)
        depths (map #(get m %) coords)
        risks (map inc depths)]
    (reduce + risks)))

(defn size-of-basin [m accum]
  (let [expanse (->> (reduce concat (mapv neighbor-coords accum))
                     (filter #(nil? (get accum %)))
                     (filter #(some? (get m %)))
                     (filter #(< (get m %) 9))
                     (concat accum)
                     (set))
        ]
    (if (= (count expanse) (count accum))
      (count expanse)
      (size-of-basin m expanse))))



(defn solve-b [m]
  (let [coords (find-low-points m)
        sizes (mapv #(size-of-basin m (set (list %))) coords)]
    (reduce * (take 3 (sort > sizes)))))


(defn read-heightmap [file]
  (reduce read-heightmap-line {} (map list (string/split (slurp file) #"\n") (range))))

(prn (solve-a (read-heightmap "test-09")))
(prn (solve-a (read-heightmap "input-09")))

(prn (solve-b (read-heightmap "test-09")))
