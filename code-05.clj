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

