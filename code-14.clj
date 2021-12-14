(require '[clojure.string :as str])

(defn make-rule [s]
  (let [[f t] (str/split s #" -> ")]
    (list (list (keyword (str (first f))) (keyword (str (second f)))) (keyword t))))


(defn make-rule-map [ps] (reduce #(assoc %1 (first %2) (second %2)) {} (map make-rule ps)))

(defn parse-input [file]
  (let [[template rules] (str/split (slurp file) #"\n\n")
        rule-set (make-rule-map (str/split rules #"\n"))]
    (do (prn rule-set)
    (list (map keyword (map str template)) rule-set))))


(defn apply-rules [template rules]

  (loop [items template
         accu []
         prev-item nil]
    (let [cur-item (first items)]
    (cond
      (empty? items)
      accu

      (rules [prev-item cur-item])
      (recur (rest items)
             (-> accu
              (conj (rules [prev-item cur-item]))
              (conj cur-item)
              )
             cur-item)

      :else
      (recur (rest items) (conj accu cur-item) cur-item)))))


(defn calc-score [template]
  (let [freqs (frequencies template)
        min-freq (apply min (vals freqs))
        max-freq (apply max (vals freqs))]
    (- max-freq min-freq)))

(defn iterate-n [file n]
  (let [[template, rules] (parse-input file)
        result (reduce (fn [a, i] (apply-rules a rules)) template (range n))
        ]
        result))


(assert (= 1588 (calc-score (iterate-n "data/test-14.txt" 10))))
(prn (calc-score (iterate-n "data/input-14.txt" 10)))
