(require '[clojure.string :as string])

(defn read-heightmap-line [accu [line y]]
  (let [line-and-x (map list line (range))]
    (reduce (fn [tmp-accu [depth x]] (assoc tmp-accu (list x y) (Integer/parseInt (str depth)) ))
            accu 
            line-and-x)))

(defn neighbor-coords [[x y]]
  (list [(inc x) y]
        [x (inc y)]
        [(dec x) y]
        [x (dec y)]))

(defn find-low-points [m]
  (filter (fn [coord] 
            (every? #(> % (get m coord))
                    (filter some? (map m (neighbor-coords coord)))))
          (keys m)))

(defn size-of-basin [m accum]
  (let [expanse (->> (reduce concat (map neighbor-coords accum)) ; all neighbors
                     (filter #(nil? (get accum %))) ; only new
                     (filter #(some? (get m %))) ; only actually on map
                     (filter #(< (get m %) 9)) ; not walls
                     (concat accum)
                     (set))]
    (if (= expanse accum)
      (count expanse)
      (recur m expanse))))

(defn solve-a [m]
  (->> (find-low-points m)
       (map m)
       (map inc)
       (reduce +)))

(defn solve-b [m]
  (->> (find-low-points m)
       (map #(size-of-basin m (set [%])))
       (sort >)
       (take 3)
       (reduce *)))


(defn read-heightmap [file]
  (reduce read-heightmap-line {} (map list (string/split (slurp file) #"\n") (range))))

(prn (solve-a (read-heightmap "test-09")))
(prn (solve-a (read-heightmap "input-09")))

(prn (solve-b (read-heightmap "test-09")))
(prn (solve-b (read-heightmap "input-09")))
