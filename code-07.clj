(require '[clojure.string :as string])

(defn split-separated-ints [s]
  (mapv #(Integer/parseInt %) (string/split (string/trim s) #"[ ,]+")))

(defn read-separated-ints [file]
  (split-separated-ints (slurp file)))

(defn abs [n]
  (if (< n 0) (- n) n))

(defn fuel-cost-a [from to]
  (let [dist (abs (- from to))]
    dist))

(defn fuel-cost-b [from to]
  (let [dist (abs (- from to))]
    (/ (* dist (inc dist)) 2)))

(def fuel-cost fuel-cost-b)

(defn crab-move-distance [crabs pos]
  (reduce + (map (partial fuel-cost pos) crabs)))

(defn iterate-crabs [crabs]
  (let [from (apply min crabs)
        to (inc (apply max crabs))]
    (map (partial crab-move-distance crabs) (range from to))))


(apply min (iterate-crabs (split-separated-ints "16,1,2,0,4,2,7,1,2,14")))
(apply min (iterate-crabs (read-separated-ints "input-07")))


