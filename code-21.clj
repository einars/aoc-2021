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


(def next-move
  {:roll-1-1 :roll-1-2
   :roll-1-2 :roll-1-3
   :roll-1-3 :roll-2-1
   :roll-2-1 :roll-2-2
   :roll-2-2 :roll-2-3
   :roll-2-3 :roll-1-1})

(def dirac-play)
(defn memo-dirac-play [move p1-space p1-score p2-space p2-score]
  (cond
    (>= p2-score 21) [0 1]
    (>= p1-score 21) [1 0]

    (or (= move :roll-1-1) (= move :roll-1-2) (= move :roll-1-3))
    (let [m1 (clamp-one (+ p1-space 1) 10)
          m2 (clamp-one (+ p1-space 2) 10)
          m3 (clamp-one (+ p1-space 3) 10)]
      (map + 
           (dirac-play (next-move move) m1 (if (= move :roll-1-3) (+ p1-score m1) p1-score) p2-space p2-score)
           (dirac-play (next-move move) m2 (if (= move :roll-1-3) (+ p1-score m2) p1-score) p2-space p2-score)
           (dirac-play (next-move move) m3 (if (= move :roll-1-3) (+ p1-score m3) p1-score) p2-space p2-score)))
    (or (= move :roll-2-1) (= move :roll-2-2) (= move :roll-2-3))
    (let [m1 (clamp-one (+ p2-space 1) 10)
          m2 (clamp-one (+ p2-space 2) 10)
          m3 (clamp-one (+ p2-space 3) 10)]
      (map + 
           (dirac-play (next-move move) p1-space p1-score m1 (if (= move :roll-2-3) (+ p2-score m1) p2-score))
           (dirac-play (next-move move) p1-space p1-score m2 (if (= move :roll-2-3) (+ p2-score m2) p2-score))
           (dirac-play (next-move move) p1-space p1-score m3 (if (= move :roll-2-3) (+ p2-score m3) p2-score))))))

(def dirac-play (memoize memo-dirac-play))


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

;(prn (dirac-play :roll-1-1 4 0 8 0))
(prn (apply max (dirac-play :roll-1-1 8 0 10 0)))

