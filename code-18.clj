(require '[clojure.string :as str])
(require '[clojure.tools.trace :use all])


(defn parse-snail [s] (read-string s)) ; just use clojure edn parser

(defn parse-problem [s] (map parse-snail (str/split s #"\n")))

(defn adjust-back [s adj]
  (if (number? s)
    (+ adj s)
    (let [[l r] s]
      (list l (adjust-back r adj)))))

(defn reduce-snail
  ([s mode] (reduce-snail s 0 0 mode))
  ([s depth adj-fwd mode]
   (if (number? s)
     (let [n (+ s adj-fwd)]
       (if (and (> n 9) (= mode :split))
         [(list (quot n 2) (int (Math/ceil (/ n 2)))) 0 0 :do-nothing]
         [n 0 0 mode]))
     (if (and (>= depth 4) (number? (first s)) (number? (second s)) (= mode :explode))
       [0 (first s) (second s) :do-nothing]
       (let [[l r] s
             [l adj-back-1 adj-fwd mode] (reduce-snail l (inc depth) adj-fwd mode) ; adj-back-1 atgriezīsim izsaucējam
             [r adj-back-2 adj-fwd mode] (reduce-snail r (inc depth) adj-fwd mode)
             l (if (> adj-back-2 0) (adjust-back l adj-back-2) l)]
         [(list l r) adj-back-1 adj-fwd mode])))))

(defn exec-snail [s] 
  (let [new-s (first (reduce-snail s :explode))]
    (if (= new-s s)
      (let [new-s (first (reduce-snail s :split))]
        (if (= new-s s)
          new-s
          (recur new-s)))
      (recur new-s))))

(defn add-snail [s1 s2]
  (exec-snail (list s1 s2)))

(defn magnitude [s] 
  (if (number? s) s
    (let [[l r] s] 
      (+ (* 3 (magnitude l))
         (* 2 (magnitude r))))))

(defn largest-sum [ss]
  (apply max (for [a ss
                   b ss]
               (if (= a b) 0 (magnitude (add-snail a b))))))

; ---

(assert (= [[[[0,9],2],3],4] (exec-snail (parse-snail "[[[[[9,8],1],2],3],4] "))))

(assert (= [7,[6,[5,[7,0]]]] (exec-snail (parse-snail "[7,[6,[5,[4,[3,2]]]]]"))))

(assert (= [[6,[5,[7,0]]],3] (exec-snail (parse-snail "[[6,[5,[4,[3,2]]]],1]"))))

(assert (= [[[[0,7],4],[[7,8],[6,0]]],[8,1]] (add-snail
                                               (parse-snail "[[[[4,3],4],4],[7,[[8,4],9]]]")
                                               (parse-snail "[1,1]"))))

(assert (= 3488 (magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])))

(assert (= 4140 (magnitude (reduce add-snail [
                                              [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                                              [[[5,[2,8]],4],[5,[[9,9],0]]]
                                              [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                                              [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                                              [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                                              [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                                              [[[[5,4],[7,7]],8],[[8,3],8]]
                                              [[9,3],[[9,9],[6,[4,9]]]]
                                              [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                                              [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
                                              ]))))

(assert (= 3993 (largest-sum [
                              [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                              [[[5,[2,8]],4],[5,[[9,9],0]]]
                              [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                              [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                              [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                              [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                              [[[[5,4],[7,7]],8],[[8,3],8]]
                              [[9,3],[[9,9],[6,[4,9]]]]
                              [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                              [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
                              ])))

(prn "magnitude" (magnitude (reduce add-snail (parse-problem (slurp "data/input-18.txt")))))
(prn "largest-sum" (largest-sum (parse-problem (slurp "data/input-18.txt"))))
