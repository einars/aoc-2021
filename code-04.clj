(require '[clojure.string :as string])

(defn read-strings [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (reduce conj [] (line-seq rdr))))

(defn read-ints [file]
  (mapv #(Integer/parseInt %) (read-strings file)))

(defn split-separated-ints [s]
  (mapv #(Integer/parseInt %) (string/split (string/trim s) #"[ ,]+")))

(defn read-separated-ints [file]
  (split-separated-ints (slurp file)))

(defn is-completed? [board]
  (boolean (some #{[]} (get board :combos))))

(defn make-board [s]
  (let [ lines (mapv split-separated-ints (string/split (string/trim s) #"\n"))]
    {
     ;:initial lines
     :combos (concat lines (apply (partial mapv vector) lines))
     :remaining (reduce concat lines)
     :guesses []
     }))

(defn apply-placed-number [n combos]
  (mapv #(filterv (partial not= n) %) combos))

(defn board-place [n board]
  (merge board {
                :combos (apply-placed-number n (get board :combos))
                :remaining (filter (partial not= n) (get board :remaining))
                :guesses (conj (get board :guesses) n)
                }))



(defn append-score [board]
  (merge board {
                :score (* (last (get board :guesses))
                          (reduce + (get board :remaining)))
                }))

(defn run-matches [boards numbers]
  (if (some is-completed? boards)
    (map append-score (filter is-completed? boards))
    (let [ n (first numbers) ]
      (run-matches (mapv #(board-place n %) boards) (rest numbers)))))


(defn run-matches-until-last [boards numbers]
  (let [ n (first numbers) 
          new-boards (map #(board-place n %) boards) ]
    (if (some #(not (is-completed? %)) new-boards)
      (run-matches-until-last (filter #(not (is-completed? %)) new-boards) (rest numbers))
      (map append-score new-boards))))

(defn split-into-boards [s]
  (mapv make-board (string/split s #"\n\n")))

(do
  (def a04tn (read-separated-ints "data/test-04-numbers"))
  (def a04tb (split-into-boards (slurp "data/test-04-boards")))
  (run-matches a04tb a04tn))

(let [nx (read-separated-ints "data/input-04-numbers")
      bx (split-into-boards (slurp "data/input-04-boards"))
      res (run-matches bx nx)]
  (do (println res)
      res))

(let [nx (read-separated-ints "data/input-04-numbers")
      bx (split-into-boards (slurp "data/input-04-boards"))
      res (run-matches-until-last bx nx)]
  (do (println res)
      res))

(let [nx (read-separated-ints "test-04-numbers")
      bx (split-into-boards (slurp "test-04-boards"))
      res (run-matches-until-last bx nx)]
  (do (println res)
      res))
