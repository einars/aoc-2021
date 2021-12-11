; :FireplaceConnect 33333

(defn read-strings [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (reduce conj [] (line-seq rdr))))

(defn read-ints [file]
  (mapv #(Integer/parseInt %) (read-strings file)))


(defn aoc01 []
  (let [numbers (read-ints "data/input-01a")]
    (count (filter #(< (first %) (second %)) (partition 2 1 numbers)))))

; 1292
(aoc01)

(defn aoc01b []
  (let [numbers (read-ints "data/input-01a")
        sums (mapv #(+ (first %) (second %) (nth % 2)) (partition 3 1 numbers))]
    (count (filter #(< (first %) (second %)) (partition 2 1 sums)))))

(aoc01b)
; 1262


(defn aoc02-navigate [ pos_and_depth line ]
  (let [
        parts (clojure.string/split line #" ")
        command (first parts)
        move_depth (Integer/parseInt (second parts))]
    (condp = command
     "forward" (seq [(+ (first pos_and_depth) move_depth) (second pos_and_depth)])
     "up" (seq [(first pos_and_depth) (- (second pos_and_depth) move_depth)])
     "down" (seq [(first pos_and_depth) (+ (second pos_and_depth) move_depth)])
    )))

(defn print-answer-02 [ pos_and_depth ]
  (let [ans (* (first pos_and_depth) (second pos_and_depth))]
    (do
      (println "pos " (first pos_and_depth))
      (println "depth " (first pos_and_depth))
      (println "ans " ans)
      pos_and_depth)))

(defn aoc02 []
  (let [lines (read-strings "data/input-02")]
    (print-answer-02 (reduce aoc02-navigate '(0 0) lines))))

(defn aoc02b-navigate [ pos_depth_aim line ]
  (let [
        parts (clojure.string/split line #" ")
        command (first parts)
        n (Integer/parseInt (second parts))
        pos (first pos_depth_aim)
        depth (second pos_depth_aim)
        aim (nth pos_depth_aim 2)]
    (condp = command
     "forward" (seq [(+ n pos) (+ depth (* aim n)) aim])
     "up" (seq [pos depth (- aim n)])
     "down" (seq [pos depth (+ aim n)]))
    ))

;(def aoc02-test [
;  "forward 5"
;  "down 5"
;  "forward 8"
;  "up 3"
;  "down 8"
;  "forward 2" ])
;
;(print-answer-02 (reduce aoc02b-navigate '(0 0 0) aoc02-test))

(defn aoc02b []
  (let [lines (read-strings "data/input-02")]
    (print-answer-02 (reduce aoc02b-navigate '(0 0 0) lines))))


;;;; 3

(defn chars-to-ints [line]
  (mapv #(Character/digit % 10) line))

(defn map-sum-2 [x1 x2] (map + x1 x2))

(defn bin-to-dec [bits] 
  (first (reduce (fn [work bit] 
            (let [accum (first work)
                  multiplier (second work)]
                [ (+ accum (if (= 1 bit) multiplier 0)) (* multiplier 2)]))
          [0 1] (reverse bits))))

(assert (= 10 (bin-to-dec [1 0 1 0])))

(defn gamma-binary [numbers] 
(let [
      f (first numbers)
      bit-counts (reduce map-sum-2 f (rest numbers))
      max-bits (count numbers) ]
    (let [gamma (map #(if (> % (- max-bits %)) 1 0) bit-counts)
          epsilon (map #(if (< % (- max-bits %)) 1 0) bit-counts)]
      [ (bin-to-dec gamma) (bin-to-dec epsilon) ]
    )
))

(let [input [ "00100" "11110" "10110" "10111" "10101"
              "01111" "00111" "11100" "10000" "11001"
              "00010" "01010"]]
  (assert (= [22 9] (gamma-binary (map chars-to-ints input)))))


(defn aoc3-most-bit-at-pos [numbers pos cmp?]
  (let [
       bits (map #(nth % pos) numbers)
       n-ones (reduce + bits)
       max-bits (count bits)
       ]
    (if (= n-ones (- max-bits n-ones))
      (if (cmp? 1 0) 1 0) ; 0 == 1
      (if (cmp? n-ones (- max-bits n-ones)) 1 0))))

(assert (= (aoc3-most-bit-at-pos (map chars-to-ints ["0" "0" "0" "1"]) 0 >) 0))
(assert (= (aoc3-most-bit-at-pos (map chars-to-ints ["0" "0" "0" "1"]) 0 <) 1))
(assert (= (aoc3-most-bit-at-pos (map chars-to-ints ["1" "1" "1" "0"]) 0 >) 1))
(assert (= (aoc3-most-bit-at-pos (map chars-to-ints ["1" "1" "1" "0"]) 0 <) 0))
(assert (= (aoc3-most-bit-at-pos (map chars-to-ints ["0" "1"]) 0 >) 1))
(assert (= (aoc3-most-bit-at-pos (map chars-to-ints ["0" "1"]) 0 <) 0))


(defn aoc3-filter [numbers pos cmp?] 
  (let [
        preferred-bit (aoc3-most-bit-at-pos numbers pos cmp?)
        filtered (filter #(= (nth % pos) preferred-bit) numbers)
        ]
    (if (= (count filtered) 1)
      (bin-to-dec (first filtered))
      (aoc3-filter filtered (+ 1 pos) cmp?))))

(defn aoc3-oxygen [numbers]
  (aoc3-filter numbers 0 >))

(defn aoc3-co2 [numbers]
  (aoc3-filter numbers 0 <))

(defn oxygen-co2 [numbers]
  [ (aoc3-oxygen numbers) (aoc3-co2 numbers) ])



(let [input [ "00100" "11110" "10110" "10111" "10101"
              "01111" "00111" "11100" "10000" "11001"
              "00010" "01010"]]
  (do
    (assert (= 23 (aoc3-oxygen (map chars-to-ints input))))
    (assert (= 10 (aoc3-co2 (map chars-to-ints input))))))


(defn aoc03 []
  (let [lines (read-strings "data/input-03")
        binaries (map chars-to-ints lines)
        ge (gamma-binary binaries)
        gamma (first ge)
        eps (second ge)]
    (* gamma eps)))


(defn aoc03b []
  (let [lines (read-strings "data/input-03")
        binaries (map chars-to-ints lines)
        x (oxygen-co2 binaries)
        oxygen (first x)
        co2 (second x)]
    (* oxygen co2)))



(defn exec [func_name]
  (do
    (println func_name (apply (resolve (symbol func_name)) []))
    (println "---")))

(exec "aoc01")
(exec "aoc01b")
(exec "aoc02")
(exec "aoc02b")
(exec "aoc03")
(exec "aoc03b")
