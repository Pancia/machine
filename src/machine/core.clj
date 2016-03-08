(ns machine.core)

(defprotocol IRobot
  (log [auto f|msg] "adds a msg, or (f auto), to the auto's history")
  (transition [auto f|s] "updates the state the machine is in by using a function or a literal state")
  (advance [auto which] "advances the tapes of the automata. :all updates all, whereas a number indicates the position of the tape to advance")
  (accept [auto] "accepts the machine, stops iteration")
  (reject [auto msg] "rejects the machine with the given msg")
  (value [auto f|v] "updates the value by using a fn or a value"))

(defn ?f->fn [?f] (if (fn? ?f) ?f (fn [_] ?f)))

(defrecord Machine [value steps state tapes history accept-states]
  IRobot
  (log [auto f|msg]
    (update auto :history
            #(if-let [msg (cond
                            (= f|msg ::tapes) (if (every? seq (:tapes auto))
                                                (->> auto :tapes (mapv first)))
                            (fn? f|msg)       (f|msg auto)
                            :else             f|msg)]
              (conj % msg) %)))
  (transition [auto ?f]
    (-> auto (update-in [:state] (?f->fn ?f))
        (.log #(str '-> (name (:state %))))))
  (advance [auto which]
    (cond-> auto
      (= which :all)  (update-in [:tapes] #(mapv next %))
      ;TODO: multiple which eg: [0 2 3] out of [0 1 2 3]
      (number? which) (update-in [:tapes which] next)

      true (.log ::tapes)))
  (accept [auto]
    (-> auto
        (transition :accepted)))
  (reject [auto msg]
    (-> auto
        (assoc :rejected/msg msg)
        (transition :rejected)))
  (value [auto ?f]
    (update auto :value (?f->fn ?f))))

(defn pretty-tape [tape]
  (str "<-" tape "-<"))

(defmethod print-method
  Machine [auto w]
  (print-method "#machine" w)
  (print-method
    (-> auto (select-keys [:value :state :tapes :history :accept-states :rejected/msg])
        (update :tapes (partial map pretty-tape))) w))

(defn build
  ([steps] (build {} steps))
  ([value {:keys [accept-states on-empty] :as steps}]
   (let [steps' (dissoc steps :accept-states :on-empty)]
     (map->Machine {:value value :steps steps' :history []
                    :state :normal :tapes [] :on-empty (or on-empty
                                                           (fn [& _] nil))
                    :accept-states (or accept-states
                                       (-> steps' keys set))}))))

(defn compose [auto & autos]
  (reduce (fn [acc {:keys [steps value]}]
            (-> acc
                (update :steps (partial merge-with comp) steps)
                (update :value merge value)))
          auto autos))

(defn machine? [?auto]
  (-> ?auto type (= Machine)))

(defn finish [{:keys [accept-states on-empty tapes] :as auto}]
  (-> (or (when (some identity tapes)
            (when-let [after-empty (apply (partial on-empty auto) tapes)]
              (when (machine? after-empty)
                after-empty)))
          auto)
      (update :state #(if (-> accept-states set (conj :accepted) %)
                        :accepted :rejected))))

(defn run [auto & tapes]
  (letfn [(final-state? [{:keys [state]}]
            (#{:accepted :rejected} state))
          (exec-step [{:keys [steps state] :as auto}]
            ((get steps state) auto))]
    (->> (assoc auto :tapes (vec tapes))
      (#(.log % ::tapes))
      (iterate (fn [{:as auto :keys [tapes]}]
                 (if (some empty? tapes)
                   (finish auto)
                   (exec-step auto))))
      (drop-while (comp not final-state?))
      first)))
