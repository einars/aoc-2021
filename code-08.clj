(require '[clojure.string :as string])

(defn split-separated-strings [s]
  (string/split (string/trim s) #"[ ,]+"))


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
  (let [numbers (map #(list (first %) (count (second %)) ) number-mapping)] 
    (map first (filter #(= n (second %)) numbers))))


(defn recur-test [tried-mappings input reqd constraints accum]
  (if (empty? input)
    [tried-mappings (conj accum tried-mappings)]
    (let [source (first input)
          possible-targets-1 (filter #(nil? (% (set (vals tried-mappings)))) reqd)
          possible-targets (filter #(some? (% (source constraints))) possible-targets-1)
          ]
      (if (empty? possible-targets)
        [tried-mappings accum] ; failed
        (loop [new-accum accum
               targets possible-targets]
          (if (empty? targets)
            [tried-mappings new-accum]
            (let [result (recur-test (assoc tried-mappings source (first targets))
                                     (rest input)
                                     reqd
                                     constraints
                                     new-accum)]
              (recur (second result) (rest targets)))))))))


(defn apply-mapping [constraints mapping]
  (reduce #(if (some? (%2 mapping))
             (assoc %1 %2 (set [(%2 mapping)]))
             (assoc %1 %2 (reduce disj (%2 %1) (vals mapping)) ))
          constraints all-segments))



(defn possible-mappings [input candidate constraints]
  (map #(apply-mapping constraints %)
       (second (recur-test {} input (candidate number-mapping) constraints []))))


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
    (or (= :1 elt)
        (= :4 elt)
        (= :7 elt)
        (= :8 elt)))

(defn count-1478 [line]
  (reduce + (map #(if (is-1478 %) 1 0) line)))

(defn line-as-int [solved]
  (Integer/parseInt (string/join "" (map name solved))))


(println (reduce + (map count-1478 (map solve-line (string/split (slurp "test-08") #"\n")))))
(println (reduce + (map count-1478 (map solve-line (string/split (slurp "input-08") #"\n")))))

(println (map line-as-int (map solve-line (string/split (slurp "test-08") #"\n"))))
(println (reduce + (map line-as-int (map solve-line (string/split (slurp "input-08") #"\n")))))

