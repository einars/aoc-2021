(require '[clojure.test :refer [are is]])
(require '[clojure.tools.trace :use all])
(require '[clojure.string :as str])


(defn make-coordinate-map [[line y]]
  (let [line-and-x (map list line (range))]
    (reduce (fn [accu [elem x]] (if (not= elem \.) 
                                  (assoc accu (list x y) (str elem))
                                  accu))
            {}
            line-and-x)))

(defn read-xy-map [file]
  (let [xy-map (reduce merge (map make-coordinate-map (map list (str/split (slurp file) #"\n") (range))))
        size-x (apply max (map first (keys xy-map)))
        size-y (apply max (map second (keys xy-map)))
        ]
    [xy-map [(inc size-x) (inc size-y)]]))

(defn pos-in-direction [[x y] cucumber [size-x size-y]]
  (if (= ">" cucumber)
    [(mod (inc x) size-x) y]
    [x (mod (inc y) size-y)]))

(defn map-step-impl [m size cucus]
  (into {} (map (fn [[pos cucumber]]
                  (if (= cucumber cucus)
                    (let [next-pos (pos-in-direction pos cucumber size)]
                      (if (nil? (m next-pos))
                        [next-pos cucumber]
                        [pos cucumber]))
                    [pos cucumber]))
                (seq m))))

(defn map-step [m size]
  (map-step-impl (map-step-impl m size ">") size "v"))


(defn calc-stop-point 
  ([[m size]] (calc-stop-point m size 1))
  ([m size n]
   (let [nm (map-step m size)]
     (printf ".")
     (flush)
     (if (= nm m)
       n
       (recur nm size (inc n))))))

(defn print-map [m [size-x size-y]]
  (doseq [y (range size-x)
          x (range size-y)]
      (when (= x 0)
        (printf "\n"))
      (printf "%s" (m [x y] ".")))
  (prn))


(assert (= 58 (calc-stop-point (read-xy-map "data/test-25.txt"))))
(prn (calc-stop-point (read-xy-map "data/input-25.txt")))
