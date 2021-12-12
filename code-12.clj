(require '[clojure.string :as str])
(use 'clojure.tools.trace)


(defn read-paths [file]
  (set (map set (map #(str/split % #"-") (str/split (slurp file) #"\n")))))

(defn may-visit-a? [cave path-traversed]
  (if (= cave "start") 
    false
    (let [first-letter (str (first cave))]
      (if (= first-letter (str/upper-case first-letter))
        true
        (empty? (filter (partial = cave) path-traversed))))))

(defn is-small-cave? [cave]
  (let [first-letter (str (first cave))]
    (= first-letter (str/lower-case first-letter))))

(def is-big-cave? (complement is-small-cave?))

(defn has-repeated-visits? [path-traversed]
  (let [small-caves (filter is-small-cave? path-traversed)]
    (> (count small-caves)
       (count (set small-caves)))))

(defn may-visit-b? [cave path-traversed]
  (if (= cave "start") 
    false
    (if (is-big-cave? cave)
      true
      (or
        (empty? (filter (partial = cave) path-traversed))
        ((complement has-repeated-visits?) path-traversed)))))

(def ^:dynamic *may-visit-fn* may-visit-a?)

(defn reachable-caves [location paths path-traversed]
  (let [paths-including-self (filter #(% location) paths)
        destinations (map #(first (disj % location)) paths-including-self)]
    (filter #(*may-visit-fn* % path-traversed) destinations)))


(defn find-paths
  ([paths] (find-paths paths [] "start" []))
  ([paths path-so-far current-location results]
   (if (= current-location "end")
     (conj results path-so-far) ; success
     (reduce concat (for [cave (reachable-caves current-location paths path-so-far)]
                      (find-paths paths (conj path-so-far cave) cave []))))))

; ---

(binding [*may-visit-fn* may-visit-a?]
  (assert (= 10 (count (find-paths (read-paths "data/test-12a.txt")))))
  (assert (= 19 (count (find-paths (read-paths "data/test-12b.txt")))))
  (assert (= 226 (count (find-paths (read-paths "data/test-12c.txt")))))
  (prn (count (find-paths (read-paths "data/input-12.txt")))))

(binding [*may-visit-fn* may-visit-b?]
  (assert (= 36 (count (find-paths (read-paths "data/test-12a.txt")))))
  (prn (count (find-paths (read-paths "data/input-12.txt")))))
