(require '[clojure.string :as str])
(use 'clojure.tools.trace)

(def hex-map
  {\0 [0 0 0 0]
   \1 [0 0 0 1]
   \2 [0 0 1 0]
   \3 [0 0 1 1]
   \4 [0 1 0 0]
   \5 [0 1 0 1]
   \6 [0 1 1 0]
   \7 [0 1 1 1]
   \8 [1 0 0 0]
   \9 [1 0 0 1]
   \A [1 0 1 0]
   \B [1 0 1 1]
   \C [1 1 0 0]
   \D [1 1 0 1]
   \E [1 1 1 0]
   \F [1 1 1 1]})

(defn bits-to-dec
  ([bs] (bits-to-dec bs 0))
  ([bs accu]
   (if (empty? bs)
     accu
     (recur (rest bs) (+ (* 2 accu) (first bs))))))

(defn bs-take [n bs]
  (list (bits-to-dec (take n bs))
        (drop n bs)))

(defn bs-read-literal-part [bs]
  (let [[has-more bs] (bs-take 1 bs)
        [part bs] (bs-take 4 bs)]
    (list has-more part bs)))

(defn bs-read-literal
  ([bs] (bs-read-literal bs 0))
  ([bs accu]
   (let [[has-more part bs] (bs-read-literal-part bs)
         accu (+ (* 16 accu) part)]
     (if (= 1 has-more)
       (recur bs accu)
       (list accu bs)))))

(def parse-bitstream) ; forward

(defn parse-bitstream-until-end
  ([bs] (parse-bitstream-until-end bs []))
  ([bs accu]
   (if (< (count bs) 6)
     accu
     (let [[packet bs] (parse-bitstream bs)]
       (recur bs (conj accu packet))))))

(defn parse-bitstream-until-n
  ([bs n] (parse-bitstream-until-n bs n []))
  ([bs n accu]
   (if (= n 0)
     [accu bs]
     (let [[packet bs] (parse-bitstream bs)]
       (recur bs (dec n) (conj accu packet))))))


(defn parse-bitstream [bs]
  (let [[version bs] (bs-take 3 bs)
        [packet-type bs] (bs-take 3 bs) ]
    (if (= 4 packet-type)
      (let [[literal bs] (bs-read-literal bs)]
        (list {:version version, :packet-type packet-type, :literal literal, :sub (list)}
              bs))
      (let [[ltp bs] (bs-take 1 bs)]
        (if (= ltp 0)
          (let [[len-sub bs] (bs-take 15 bs)
                subpackets (parse-bitstream-until-end (take len-sub bs))
                bs (drop len-sub bs)]
            (list {:version version, :packet-type packet-type, :sub subpackets } 
                  bs))
          (let [[n-subpackets bs] (bs-take 11 bs)
                [subpackets bs] (parse-bitstream-until-n bs n-subpackets)]
            (list {:version version, :packet-type packet-type, :sub subpackets } 
                  bs)))))))

(defn sum-versions
  ([ps] (sum-versions ps 0))
  ([ps accu]
   (if (empty? ps)
     accu
     (recur (rest ps) (+ accu 
                         (:version (first ps)) 
                         (sum-versions (:sub (first ps)) 0))))))



(defn make-bitstream [s] (mapcat hex-map s))

(defn solve-a [s]
  (->> (make-bitstream s)
       (parse-bitstream-until-end)
       ;(parse-bitstream)
       (sum-versions)))

(defn format-packet [p]
   (format "%d:%d%s" (:version p) (:packet-type p)
           (if (empty? (:sub p))
             (format "=%d" (:literal p))
             (format ":(%s)" (str/join " " (map format-packet (:sub p)))))))

(assert (= 16 (solve-a "8A004A801A8002F478")))
(assert (= 12 (solve-a "620080001611562C8802118E34")))
(assert (= 23 (solve-a "C0015000016115A2E0802F182340")))
(assert (= 31 (solve-a "A0016C880162017C3686B18A3D4780")))
(prn (solve-a (slurp "data/input-16.txt")))
