(require '[clojure.test :refer [are is]])
(require '[clojure.tools.trace :use all])

(def rooms
  {0  {:nbs [1],      :home nil, :hallway true}
   1  {:nbs [0 2],    :home nil, :hallway true}
   3  {:nbs [2 4],    :home nil, :hallway true}
   5  {:nbs [4 6],    :home nil, :hallway true}
   7  {:nbs [6 8],    :home nil, :hallway true}
   9  {:nbs [8 10],   :home nil, :hallway true}
   10 {:nbs [9],      :home nil, :hallway true}
   11 {:nbs [[1] [3] 12], :home 'a}
   12 {:nbs [11],         :home 'a, :endpoint true}
   13 {:nbs [[3] [5] 14], :home 'b}
   14 {:nbs [13],         :home 'b, :endpoint true}
   15 {:nbs [[5] [7] 16], :home 'c}
   16 {:nbs [15],         :home 'c, :endpoint true}
   17 {:nbs [[7] [9] 18], :home 'd}
   18 {:nbs [17],         :home 'd, :endpoint true}
   })

(def test-state-0
  [{:me 'b, :in 11, :spent 0, :cost 1}
   {:me 'a, :in 12, :spent 0, :cost 1}
   {:me 'c, :in 13, :spent 0, :cost 10}
   {:me 'd, :in 14, :spent 0, :cost 10}
   {:me 'b, :in 15, :spent 0, :cost 100}
   {:me 'c, :in 16, :spent 0, :cost 100}
   {:me 'd, :in 17, :spent 0, :cost 1000}
   {:me 'a, :in 18, :spent 0, :cost 1000}])

(defn unoccupied-rooms [room_ids state]
  (filter #(every? (fn [st] (not= (:in st) %)) state) room_ids))

(is (= [9 10] (unoccupied-rooms [9 10 11 12] test-state-0)))


(defn foreign-colors-in-my-room? [me others rooms]
  (some (fn [o] (and (not= me (:me o))
                     (= me (:home (rooms (:in o))))) others)))



(defn available-moves
  ([state rooms] (mapcat #(available-moves % (filter (partial not= %) state) rooms) state))
  ([self others rooms]
   (let [this-room (rooms (:in self))
         unoccupied (unoccupied-rooms (:nbs this-room) others)]
     (loop [nbs unoccupied
            accu []]
       (if (empty? nbs)
         accu
         (let [nb-room (first nbs)
               ; list syntax for neighboring room means that it is a double-step room
               mult (if (coll? nb-room) 2 1)
               nb-room-id (if (coll? nb-room) (first nb-room) nb-room)
               nb-room (rooms nb-room-id)]
           (cond
             (and
               (= (:home this-room) (:me self))
               (:endpoint this-room))
             ; do not move out of own endpoint
             (recur (rest nbs) accu)

             ;(and
             ;  (= (:home this-room) (:me self))
             ;  (nil? (:endpoint nb-room)))
             ;; do not move out of own room
             ;(do (prn "no-move-out-of-own")
             ;(recur (rest nbs) accu))

             (and (:hallway? this-room)
                  (not= (:home nb-room) (:me self)))
             ; do not move into other rooms
             (recur (rest nbs) accu)

             (and (:hallway? this-room)
                  (= (:home nb-room) (:me self))
                  (foreign-colors-in-my-room? (:me self) others rooms))
             ; do not move into own room taken by others
             (recur (rest nbs) accu)

             ;todo: do not move into room if it contains 

             :else
             (recur (rest nbs) 
                    (conj accu
                          (conj others {:me (:me self)
                                        :in nb-room-id
                                        :spent (+ (:spent self) (* mult (:cost self)))
                                        :cost (:cost self)
                                        }))))))))))

(defn is-winning-state? [state rooms]
  (every? (fn [me] (= (:me me) 
                      (:home (rooms (:in me)))))
          state))

(defn advance-state [states rooms]
  (let [new-states (mapcat #(available-moves % rooms) states)
        winners (filter #(is-winning-state? % rooms) new-states)]
    (assert (seq new-states))
    (prn (count new-states))
    (if (empty? winners)
      (recur new-states rooms)
      (do
        (prn winners)
        (assert false)))))


(advance-state [test-state-0] rooms)
