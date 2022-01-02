(require '[clojure.test :refer [are is]])
(require '[clojure.tools.trace :use all])

(require '[shams.priority-queue :as pq])

;(def rooms ; global. if it changes in part 2, i'll deal w/that then
;  {1  {:id 1,  :nbs [2],       :home 'hallway}
;   2  {:id 2,  :nbs [1 3],     :home 'hallway}
;   3  {:id 3,  :nbs [2 4 12],  :home 'hallway, :move-through true}
;   4  {:id 4,  :nbs [3 5],     :home 'hallway}
;   5  {:id 5,  :nbs [4 6 14],  :home 'hallway, :move-through true}
;   6  {:id 6,  :nbs [5 7],     :home 'hallway}
;   7  {:id 7,  :nbs [6 8 16],  :home 'hallway, :move-through true}
;   8  {:id 8,  :nbs [7 9],     :home 'hallway}
;   9  {:id 9,  :nbs [8 10 18], :home 'hallway, :move-through true}
;   10 {:id 10, :nbs [9 11],    :home 'hallway}
;   11 {:id 11, :nbs [10],      :home 'hallway}
;   12 {:id 12, :nbs [3 13],    :home 'a}
;   13 {:id 13, :nbs [12],      :home 'a}
;   14 {:id 14, :nbs [5 15],    :home 'b}
;   15 {:id 15, :nbs [14],      :home 'b}
;   16 {:id 16, :nbs [7 17],    :home 'c}
;   17 {:id 17, :nbs [16],      :home 'c}
;   18 {:id 18, :nbs [9 19],    :home 'd}
;   19 {:id 19, :nbs [18],      :home 'd}
;   })
;
;(def pod-state-0
;  {0 {:me 'a, :in 12, :spent 0, :cost 1}
;   1 {:me 'a, :in 17, :spent 0, :cost 1}
;   2 {:me 'b, :in 16, :spent 0, :cost 10}
;   3 {:me 'b, :in 18, :spent 0, :cost 10}
;   4 {:me 'c, :in 14, :spent 0, :cost 100}
;   5 {:me 'c, :in 19, :spent 0, :cost 100}
;   6 {:me 'd, :in 13, :spent 0, :cost 1000}
;   7 {:me 'd, :in 15, :spent 0, :cost 1000}})
;


(def rooms ; hello part 2
  {1  {:id 1,  :nbs [2],       :home 'hallway}
   2  {:id 2,  :nbs [1 3],     :home 'hallway}
   3  {:id 3,  :nbs [2 4 12],  :home 'hallway, :move-through true}
   4  {:id 4,  :nbs [3 5],     :home 'hallway}
   5  {:id 5,  :nbs [4 6 14],  :home 'hallway, :move-through true}
   6  {:id 6,  :nbs [5 7],     :home 'hallway}
   7  {:id 7,  :nbs [6 8 16],  :home 'hallway, :move-through true}
   8  {:id 8,  :nbs [7 9],     :home 'hallway}
   9  {:id 9,  :nbs [8 10 18], :home 'hallway, :move-through true}
   10 {:id 10, :nbs [9 11],    :home 'hallway}
   11 {:id 11, :nbs [10],      :home 'hallway}
   12 {:id 12, :nbs [3 13],    :home 'a}
   13 {:id 13, :nbs [12 20],   :home 'a}
   14 {:id 14, :nbs [5 15],    :home 'b}
   15 {:id 15, :nbs [14 21],   :home 'b}
   16 {:id 16, :nbs [7 17],    :home 'c}
   17 {:id 17, :nbs [16 22],   :home 'c}
   18 {:id 18, :nbs [9 19],    :home 'd}
   19 {:id 19, :nbs [18 23],   :home 'd}

   20 {:id 20, :nbs [13 24],   :home 'a}
   21 {:id 21, :nbs [15 25],   :home 'b}
   22 {:id 22, :nbs [17 26],   :home 'c}
   23 {:id 23, :nbs [19 27],   :home 'd}
   24 {:id 24, :nbs [20],      :home 'a}
   25 {:id 25, :nbs [21],      :home 'b}
   26 {:id 26, :nbs [22],      :home 'c}
   27 {:id 27, :nbs [23],      :home 'd}
   })

(def pod-state-0
  {0  {:me 'a, :in 12, :spent 0, :cost 1}
   1  {:me 'a, :in 19, :spent 0, :cost 1}
   2  {:me 'a, :in 22, :spent 0, :cost 1}
   3  {:me 'a, :in 26, :spent 0, :cost 1}
   4  {:me 'b, :in 16, :spent 0, :cost 10}
   5  {:me 'b, :in 18, :spent 0, :cost 10}
   6  {:me 'b, :in 17, :spent 0, :cost 10}
   7  {:me 'b, :in 21, :spent 0, :cost 10}
   8  {:me 'c, :in 14, :spent 0, :cost 100}
   9  {:me 'c, :in 15, :spent 0, :cost 100}
   10 {:me 'c, :in 23, :spent 0, :cost 100}
   11 {:me 'c, :in 27, :spent 0, :cost 100}
   12 {:me 'd, :in 13, :spent 0, :cost 1000}
   13 {:me 'd, :in 20, :spent 0, :cost 1000}
   14 {:me 'd, :in 24, :spent 0, :cost 1000}
   15 {:me 'd, :in 25, :spent 0, :cost 1000}
   })





(defn open-areas [current-id pods visited-set]
  (let [room (rooms current-id)
        taken-rooms (set (mapv :in (vals pods)))]
    (->> (rooms current-id)
         :nbs
         (filter (complement visited-set))
         (filter (complement taken-rooms)))))

(defn unfiltered-destinations 
  "returns a list of [room-id, steps] that this pod can move to"
  ([pod pods] (unfiltered-destinations (:in pod) pods #{}))
  ([headroom-id pods visited-set]
   (if-let [destination-ids (open-areas headroom-id pods visited-set)]
     (reduce into
             (mapv (fn [d-id] (list d-id (inc (count visited-set))) ) destination-ids)
             (mapv #(unfiltered-destinations % pods (conj visited-set %)) destination-ids))
     [])))

(defn room-available? [pod-type pods]
  (->>
    (vals rooms)
    (filter #(= pod-type (:home %)))
    (map :id)
    (map (fn [room-id] (some #(when (= room-id (:in %)) (:me %)) (vals pods))))
    (every? (fn [maybe-pod-type]
              (or (nil? maybe-pod-type)
                  (= maybe-pod-type pod-type))))))


(defn can-move-to? [pod dest-id pods]
  (let [s-me (:me pod)
        source (rooms (:in pod))
        dest (rooms dest-id)
        type-dest (:home dest)]
    (cond
      ; do not stop in hallway blocker points
      (true? (:move-through dest))
      false

      ; do not enter other rooms
      (and
        (not= type-dest 'hallway)
        (not= type-dest s-me))
      false

      ; do not move from hallway into anything else than own empty room:
      ; 1. no move from hallway to anything that's not my own room
      (and
        (= 'hallway (:home source))
        (not= s-me (:home dest)))
      false

      ; 2. do not move to a room that's not yet empty
      (and
        (= s-me type-dest)
        (not (room-available? s-me pods)))
      false

      ; 2. do not leave own room unless absolutely required
      (and
        (= s-me (:home source))
        (room-available? s-me pods))
      false

      :else
      true)))

(defn is-winning? [pos]
  (every? (fn [pod] (= (:me pod) (:home (rooms (:in pod))))) (vals pos)))


(defn filtered-destinations [pod pods]
  (filterv #(can-move-to? pod (first %) pods) (unfiltered-destinations pod pods)))

(defn move-to [entry dest steps]
  (update
    (assoc entry :in dest)
    :spent
    ;(fn [e] (+ e steps))))
    (fn [e] (+ e (* (:cost entry) steps)))))

(defn run-pods [pods]
  (loop [pod-ids (keys pods)
         accu []]
    (let [me (pods (first pod-ids))]
      (if (empty? pod-ids)
        accu
        (recur (rest pod-ids)
               (into accu 
                     (map (fn [[dest steps]] (update pods (first pod-ids) #(move-to % dest steps))) 
                          (filtered-destinations me pods))))))))

(defn score [pods] (reduce + (map :spent (vals pods))))
(defn negscore [pods] (- (score pods)))

(defn print-pods [pods]
  (let [m (into {} (map (fn [p] [(:in p) (:me p)]) (vals pods)))]
    (printf "%s%s%s%s%s%s%s%s%s%s%s\n"
          (m 1 ".")
          (m 2 ".")
          (m 3 ".")
          (m 4 ".")
          (m 5 ".")
          (m 6 ".")
          (m 7 ".")
          (m 8 ".")
          (m 9 ".")
          (m 10 ".")
          (m 11 "."))
    (printf "  %s %s %s %s\n"
            (m 12 ".")
            (m 14 ".")
            (m 16 ".")
            (m 18 "."))
    (printf "  %s %s %s %s\n"
            (m 13 ".")
            (m 15 ".")
            (m 17 ".")
            (m 19 "."))))

;(defn repr [pods] (mapv :in (vals pods)))
(defn repr [pods] 
  (let [rev (into {} (map (fn [p] [(:in p) (:me p)]) (vals pods)))]
    (map #(rev % 'x) (range 1 28))))

(defn run-pool [pq-pool seen]
  (let [pods (peek pq-pool)
        r (repr pods)
        new-states (filter #(not (seen (repr %))) (run-pods pods))]
    (if (seen r)
      (recur (pop pq-pool) seen)
      (do
        (prn (score pods) (count pq-pool) (count seen))
        ;(print-pods pods)
        (if (is-winning? pods)
          (prn "WINNING" pods (score pods))
          (recur (into (pop pq-pool) new-states)
                 (conj seen r)))))))


(defn create-pool [pods]
  (pq/priority-queue negscore :elements [pods]))


(prn (run-pool (create-pool pod-state-0) #{}))
;(prn (filtered-destinations (first pod-state-0) pod-state-0))


