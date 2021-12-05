(require '[clojure.string :as string])
(require '[clojure.core.reducers :as reducers])

(defn read-strings [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (reduce conj [] (line-seq rdr))))

(defn read-ints [file]
  (mapv #(Integer/parseInt %) (read-strings file)))

(defn split-separated-ints [s]
  (mapv #(Integer/parseInt %) (string/split (string/trim s) #"[ ,]+")))

(defn read-separated-ints [file]
  (split-separated-ints (slurp file)))


(defn parse-vent [s]
  (mapv split-separated-ints (string/split (string/trim s) #" -> ")))


(defn move-towards [ n target ]
  (if (= n target)
    n
    (if (> n target)
      (- n 1)
      (+ n 1))))

(defn pos-move-towards [ pos finish ]
  (let [x1 (first pos)
        y1 (second pos)
        x2 (first finish)
        y2 (second finish)]
    [ (move-towards x1 x2) (move-towards y1 y2) ]))



(defn points-taken-by-vent [v]
  (let [start (first v)
         end (second v)]
    (loop [ pos start
           accu [ start ] ]
      (let [new-pos (pos-move-towards pos end)
            new-accu (conj accu new-pos)]
        (if (not= new-pos end)
          (recur new-pos new-accu)
          new-accu)))))


(defn append-overlap [overlaps item]
  (assoc overlaps item (+ 1 (overlaps item 0))))

(defn append-overlaps [overlaps items]
  (reduce append-overlap overlaps items))

(defn find-overlaps [ sets ]
  (filter #(> % 1) (vals (reduce append-overlaps {} sets))))

(defn is-vent-straight? [v]
  (let [start (first v)
        finish (second v)]
    (or
      (= (first start) (first finish))
      (= (second start) (second finish)))))

(->>
  ;(read-strings "test-05")
  (read-strings "input-05")
  (mapv parse-vent)
  ;(filter is-vent-straight?)
  (mapv points-taken-by-vent)
  (find-overlaps)
  (count))


