(require '[clojure.tools.trace :use all])

(defn clamp-one [n n-max] (inc (mod (dec n) n-max)))

(defn new-dice []
  (let [counter (atom 0)
        state (atom 0)]
    (fn [cmd]
      (condp = cmd
        :roll (do
                (swap! counter inc)
                (swap! state (fn [n] (clamp-one (inc n) 100))))
        :set-99 (swap! state (fn [_] 99))
        :count @counter))))

(defn play-with [[p1-space p1-score] [p2-space p2-score] die]
  (if (>= p1-score 1000) 
    [p1-score p2-score (die :count)]
    (if (>= p2-score 1000) 
      [p2-score p1-score (die :count)]

      (let [roll (+ (die :roll) (die :roll) (die :roll))
            p1-space (clamp-one (+ p1-space roll) 10)]
        (recur [p2-space p2-score]
               [p1-space (+ p1-score p1-space)]
               die)))))

(defn solve-a [p1 p2]
  (let [[win lose n-rolls] (play-with [p1 0] [p2 0] (new-dice))]
    (* lose n-rolls)))

; ---

(let [d (new-dice)]
  (d :roll)
  (d :roll)
  (d :roll)
  (assert (= 4 (d :roll)))
  (assert (= 4 (d :count)))
  (d :set-99)
  (assert (= 100 (d :roll)))
  (assert (= 1 (d :roll))))

(assert (= 739785 (solve-a 4 8)))

; -- tests ok --

(prn (solve-a 8 10))


