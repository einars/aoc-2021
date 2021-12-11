(require '[clojure.string :as str])

(defn make-coordinate-map [[line y]]
  (let [line-and-x (map list line (range))]
    (reduce (fn [accu [elem x]] (assoc accu (list x y) (Integer/parseInt (str elem)) ))
            {} 
            line-and-x)))

(defn read-xy-map [file]
  (reduce merge (map make-coordinate-map (map list (str/split (slurp file) #"\n") (range)))))

(defn find-neighbors [[x y]]
  (list [(inc x) y]
        [x (inc y)]
        [(dec x) y]
        [x (dec y)]
        [(dec x) (dec y)]
        [(dec x) (inc y)]
        [(inc x) (dec y)]
        [(inc x) (inc y)]))

(defn map-apply [m func]
  (into {} (map (fn [[k v]] [k (func v)]) m)))

(defn map-apply-when [m pred? func]
  (into {} (map (fn [[k v]] [k (if (pred? k)(func v) v)]) m)))


(defn increase-level [octos at-coordinates]
  (let [nbs (set (find-neighbors at-coordinates))]
    (map-apply-when octos nbs inc)))



(defn clamp-values [octos]
  (map-apply octos (fn [v] (if (> v 9) 0 v))))

(defn octostep [initial-octos]
  (loop [had-new-flashes false
         flashes #{}
         coords (keys initial-octos)
         octos (map-apply initial-octos inc)]
    (cond
      (and (empty? coords) (not had-new-flashes))
        [(clamp-values octos) (count flashes)] ; success

      (and (empty? coords) had-new-flashes)
        (recur false flashes (keys octos) octos) ; loop all over all the octos again

      :else
        (let [my-coords (first coords)
              my-level (octos my-coords)]
          (if (and (> my-level 9) ((complement flashes) my-coords)) ; have to flash, but didn't flash yet
              (recur true (conj flashes my-coords) (rest coords) (increase-level octos my-coords))
              (recur had-new-flashes flashes (rest coords) octos))))))

(defn run-octo-iterations 
  ([octos n] (run-octo-iterations octos n 0))
  ([octos n n-flashes]
   (cond 
     (= 0 n) n-flashes
     :else (let [[new-octos new-flashes] (octostep octos)]
             (recur new-octos (dec n) (+ n-flashes new-flashes))))))



(prn (run-octo-iterations (read-xy-map "data/test-11") 10))
(prn (run-octo-iterations (read-xy-map "data/input-11") 100))
