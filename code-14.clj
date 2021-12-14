(require '[clojure.string :as str])
(require '[clojure.core.memoize :as memoize])

(defn make-rule [s]
  (let [[f t] (str/split s #" -> ")]
    (list (list (keyword (str (first f))) (keyword (str (second f)))) (keyword t))))


(defn make-rule-map [ps] (reduce #(assoc %1 (first %2) (second %2)) {} (map make-rule ps)))

(defn parse-input [file]
  (let [[template rules] (str/split (slurp file) #"\n\n")
        rule-set (make-rule-map (str/split rules #"\n"))]
    (list (map keyword (map str template)) rule-set)))

(def ^:dynamic *rules* {})

(def memo-iterate-pair) ; forward-ref
(defn iterate-pair [a b n]
  "returns map of frequencies"

  (if (= n 0)
    { b 1 } ; a is already counted
    (if-let [c (*rules* [a b])]
      (merge-with +
             (memo-iterate-pair a c (dec n))
             (memo-iterate-pair c b (dec n)))
      (iterate-pair a b (dec n)))))

(def memo-iterate-pair (memoize/memo iterate-pair ))

(defn calc-score [freqs]
  (let [min-freq (apply min (vals freqs))
        max-freq (apply max (vals freqs))]
    (- max-freq min-freq)))

(defn iterate-n [file n]
  (let [[template, rules] (parse-input file)]
    (do
      (memoize/memo-clear! memo-iterate-pair)
      (binding [*rules* rules]
        (loop [items template
              prev-item nil
              freqs {}]
          (if (empty? items)
            freqs
            (recur (rest items)
                  (first items)
                  (merge-with + freqs (memo-iterate-pair prev-item (first items) n)))))))))


(assert (= 1588 (calc-score (iterate-n "data/test-14.txt" 10))))
(prn (calc-score (iterate-n "data/input-14.txt" 10)))
(prn (calc-score (iterate-n "data/input-14.txt" 40)))
