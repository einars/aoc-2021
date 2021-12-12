(require '[clojure.string :as str])
(require '[clojure.tools.trace :as trace])

(defn read-paths [file]
  (set (map set (map #(str/split % #"-") (str/split (slurp file) #"\n")))))

(defn known-destinations [paths]
  (disj (set (concat (map first paths) 
                     (map second paths)))
        "start"))

(defn reachable-caves [location paths allowed-destinations]
  (set (filter #(paths (set [location %])) allowed-destinations)))

(defn without-cave [caves cave]
  (let [first-letter (str (first cave))]
    (if (= first-letter (str/lower-case first-letter))
      (disj caves cave)
      caves)))

(defn find-paths
  ([paths] (find-paths paths (known-destinations paths) [] "start" []))
  ([paths caves-to-visit path-so-far current-location results]
   (if (= current-location "end")
     (conj results path-so-far) ; success
     (reduce concat (for [cave (reachable-caves current-location paths caves-to-visit)]
                      (find-paths paths (without-cave caves-to-visit cave) (conj path-so-far cave) cave []))))))

; ---

(assert (= 10 (count (find-paths (read-paths "data/test-12a.txt")))))
(assert (= 19 (count (find-paths (read-paths "data/test-12b.txt")))))
(assert (= 226 (count (find-paths (read-paths "data/test-12c.txt")))))
(prn (count (find-paths (read-paths "data/input-12.txt"))))
