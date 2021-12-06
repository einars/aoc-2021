(require '[clojure.string :as string])
(require '[clojure.core.reducers :as reducers])


(defn split-separated-ints [s]
  (mapv #(Integer/parseInt %) (string/split (string/trim s) #"[ ,]+")))

(defn read-separated-ints [file]
  (split-separated-ints (slurp file)))


(defn append-some-fishes-to-population [population f n]
  (assoc population f (+ n (population f 0))))

(defn append-fish-to-population [population f]
  (append-some-fishes-to-population population f 1))

(defn create-population [fs]
  (reduce append-fish-to-population {} fs))

(defn grow-single [new-population fish-entry]
  (let [age (first fish-entry)
        n (second fish-entry)]
    (if (= age 0)
      (append-some-fishes-to-population
        (append-some-fishes-to-population new-population 6 n)
        8 n)
      (append-some-fishes-to-population new-population (- age 1) n))))

(defn grow-population [population]
  (reduce grow-single {} (seq population)))


(defn grow-some-generations [population times]
  (nth (iterate grow-population population) times))


;(println (reduce + (vals (grow-some-generations (create-population (split-separated-ints "3,4,3,1,2")) 18))))
;(println (reduce + (vals (grow-some-generations (create-population (split-separated-ints "3,4,3,1,2")) 80))))
;(println (reduce + (vals (grow-some-generations (create-population (split-separated-ints "3,4,3,1,2")) 256))))

(println (reduce + (vals (grow-some-generations (create-population (read-separated-ints "input-06")) 256))))
