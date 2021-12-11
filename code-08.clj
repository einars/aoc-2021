(require '[clojure.string :as string])

(defn split-separated-strings [s]
  (string/split (string/trim s) #"[ ,]+"))

(defn filter-map [pred? m]
  (into {} (filter (fn [[k v]] (pred? v)) m)))

(def all-segments [:a :b :c :d :e :f :g])

(def initial-constraints
  {
   :a (set all-segments)
   :b (set all-segments)
   :c (set all-segments)
   :d (set all-segments)
   :e (set all-segments)
   :f (set all-segments)
   :g (set all-segments)
   })

(def number-mapping
  {
   :0 #{ :a :b :c :e :f :g }
   :1 #{ :c :f }
   :2 #{ :a :c :d :e :g }
   :3 #{ :a :c :d :f :g }
   :4 #{ :b :c :d :f }
   :5 #{ :a :b :d :f :g }
   :6 #{ :a :b :d :e :f :g }
   :7 #{ :a :c :f }
   :8 #{ :a :b :c :d :e :f :g }
   :9 #{ :a :b :c :d :f :g }
   })

(def number-mapping-inverse
  (into {} (map (fn [[k v]] [v k]) number-mapping)))

(defn of-length [n] 
  (keys (filter-map #(= n (count %)) number-mapping)))

(defn apply-mapping [constraints source target]
  (reduce (fn [c seg] (if (= seg source)
                        (assoc c source (set [target]))
                        (assoc c seg (disj (seg c) target)) )) ; remove target from all others
          constraints
          all-segments))

(defn search-constraints [input segments constraints accum]
  (if (empty? input)
    (conj accum constraints) ; success, constraints progagated to end of input
    (let [source (first input)
          possible-targets (filter #(some? (% (source constraints))) segments)]
      (if (empty? possible-targets)
        accum ; no more possible constraint placements
        (loop [new-accum accum
               targets possible-targets]
          (if (empty? targets)
            new-accum
            (let [result (search-constraints (rest input)
                                             segments
                                             (apply-mapping constraints source (first targets))
                                             new-accum)]
              (recur result (rest targets)))))))))


(defn possible-mappings [input candidate constraints]
  (search-constraints input (candidate number-mapping) constraints []))


(defn reduce-constraints [many_constraints word]
  (let [input (set (mapv (comp keyword str) word))
        alternatives (of-length (count input))]
    (reduce concat (for [a alternatives
                         c many_constraints]
                     (possible-mappings input a c)))))


(defn detect-segments [input]
  (let [constr (reduce reduce-constraints (list initial-constraints) (sort-by count < input))]
    (if (= 1 (count constr))
      (first constr)
      (do
        (println constr)
        (println "ERROR: something wrong, plz investigate")
        (assert false)))))


(def sample "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(detect-segments (take 10 (string/split sample #" ")))


(defn materialize [constraints input]
  (let [inputs (map (comp keyword str) input)
        remap (set (map #(first (% constraints)) inputs))]
    (get number-mapping-inverse remap)))


(defn solve-line [line]
  (let [inputs (string/split line #" ")
        ndef (take 10 inputs)
        nrem (drop 11 inputs)
        constraints (detect-segments ndef) ]
    (map (partial materialize constraints) nrem)))

(solve-line sample)



(defn is-1478 [elt]
  (some? (elt #{:1 :4 :7 :8})))

(defn count-1478 [line]
  (reduce + (map #(if (is-1478 %) 1 0) line)))

(defn line-as-int [solved]
  (Integer/parseInt (string/join "" (map name solved))))


(println (reduce + (map count-1478 (map solve-line (string/split (slurp "data/test-08") #"\n")))))
(println (reduce + (map count-1478 (map solve-line (string/split (slurp "data/input-08") #"\n")))))

(println (map line-as-int (map solve-line (string/split (slurp "data/test-08") #"\n"))))
(println (reduce + (map line-as-int (map solve-line (string/split (slurp "data/input-08") #"\n")))))

