(require '[clojure.string :as str])

(defn parse-parens
  ([s] (parse-parens s []))
  ([s stack]
   (let [current-char (str (first s))]
     (cond
       (empty? s) (if (empty? stack) [:ok] [:incomplete stack])
       (= current-char (first stack)) (recur (rest s) (rest stack))
       (= current-char "(") (recur (rest s) (cons ")" stack))
       (= current-char "[") (recur (rest s) (cons "]" stack))
       (= current-char "{") (recur (rest s) (cons "}" stack))
       (= current-char "<") (recur (rest s) (cons ">" stack))
       :else [:syntax current-char]))))

(def error-scores
  {")" 3
   "]" 57
   "}" 1197
   ">" 25137})

(def incomplete-scores
  {")" 1
   "]" 2
   "}" 3
   ">" 4})

(defn count-stack-score
  ([stack] (count-stack-score stack 0))
  ([stack score]
   (if (empty? stack)
     score
     (recur (rest stack) (+ (* score 5) (get incomplete-scores (first stack) 0))))))

(defn get-score [[status extra]]
  (cond
    (= status :syntax) (error-scores extra 0)
    (= status :incomplete) (count-stack-score extra)
    :else status))

(defn solve-a [file]
  (->> (str/split (slurp file) #"\n")
      (map parse-parens)
      (filter #(= (first %) :syntax))
      (map get-score)
      (reduce +)))

(defn take-element-in-middle [coll]
  {:pre [(odd? (count coll))]}
  (nth coll (/ (count coll) 2)))

(defn solve-b [file]
  (->> (str/split (slurp file) #"\n")
      (map parse-parens)
      (filter #(= (first %) :incomplete))
      (map get-score)
      (sort >)
      (take-element-in-middle)))

; ---

(assert (= [:incomplete [")"]] (parse-parens "(")))
(assert (= [:ok] (parse-parens "()")))
(assert (= [:syntax ")"] (parse-parens ")")))

(prn (solve-a "data/test-10"))
(prn (solve-a "data/input-10"))

(prn (solve-b "data/test-10"))
(prn (solve-b "data/input-10"))
