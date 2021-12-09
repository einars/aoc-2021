
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

(defn read-heightmap [file]
  (reduce read-heightmap-line {} (map list (string/split (slurp file) #"\n") (range))))

(prn (solve-a (read-heightmap "test-09")))
