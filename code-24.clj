(require '[clojure.test :refer [are is]])
(require '[clojure.tools.trace :use all])
(require '[clojure.string :as str])


(defn read-file [file]
  (map #(str/split % #" ") (str/split (slurp file) #"\n")))

(defn third [coll] (nth coll 2))

(defn parse-variable [v]
  (condp = v
    "w" :w
    "x" :x
    "y" :y
    "z" :z
    (Integer/parseInt v)))

(defn parse-command [cmd]
  (condp = (first cmd)
    "inp" (list 'inp (parse-variable (second cmd)))
    (list (symbol (first cmd))
          (parse-variable (second cmd))
          (parse-variable (third cmd)))))

(defn parse-lines [lines]
  (map parse-command lines))

(def start-state {:w 0, :x 0, :y 0, :z 0, :inp 0})

(defn get-register [state reg] (if (number? reg) reg (state reg)))

(defn execute [command state inputs break-on]
  (condp = (first command)
    'inp (if (= (:inp state) break-on)
           'enough
           (update (assoc state (second command) (nth inputs (:inp state)))
                   :inp
                   inc))
    'add (assoc state (second command) (+ (get-register state (second command)) (get-register state (third command))))
    'mul (assoc state (second command) (* (get-register state (second command)) (get-register state (third command))))
    'div (assoc state (second command) (quot (get-register state (second command)) (get-register state (third command))))
    'mod (assoc state (second command) (mod (get-register state (second command)) (get-register state (third command))))
    'eql (assoc state (second command) (if (= (get-register state (second command)) (get-register state (third command))) 1 0))))


(defn exec-code [code state inputs break-on]
  (if (empty? code) 
    (:z state)
    (let [new-state (execute (first code) state inputs break-on)]
      (if (= new-state 'enough)
        (:z state)
        (do
          (prn (first code))
          (doseq [k [:w :x :y :z]]
            (when (not= (k state) (k new-state)) (prn k (k new-state))))
          (recur (rest code) new-state inputs break-on))))))

(exec-code (parse-lines (read-file "data/input-24.txt")) start-state [1 1 1 0 0 0 0 0 0 0 0 0 0] 3)

(assert (= 0 (exec-code (parse-lines (read-file "data/input-24.txt")) start-state [9 9 9 1 9 6 9 2 4 9 6 9 3 9] nil)))
(assert (= 0 (exec-code (parse-lines (read-file "data/input-24.txt")) start-state [8 1 9 1 4 1 1 1 1 6 1 7 1 4] nil)))

