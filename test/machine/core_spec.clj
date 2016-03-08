(ns machine.core-spec
  (:require [machine.core :refer [build run compose machine?]]
            [untangled-spec.core :refer [specification component behavior assertions]]
            [clojure.test :as t]
            [clojure.set :as set]))

(defn *contains?
  ([exp-map] (fn [act-map] (*contains? act-map exp-map)))
  ([act-map exp-map]
  (set/subset? (set exp-map) (set act-map))))

(def irobot (build {}))

(specification "Machine"
  (component "IRobot"
    (component ".log"
      (assertions
        "adds a msg to :history"
        (-> irobot (.log 0))
        =fn=> (*contains? {:history [0]})
        (-> irobot (.log 0) (.log 1))
        =fn=> (*contains? {:history [0 1]})
        "logs using a fn that takes the auto"
        (-> irobot (.log #(->> % type str (re-find #"\w+$"))))
        =fn=> (*contains? {:history ["Machine"]})))
    (component ".transition"
      (assertions
        "updates :state using a fn"
        (.transition irobot #(->> % name (str "new-") keyword))
        =fn=> (*contains? {:state :new-normal})
        "or sets :state to a val"
        (.transition irobot :new-state)
        =fn=> (*contains? {:state :new-state})
        "logs the transition"
        (.transition irobot :blah)
        =fn=> (*contains? {:history ["->blah"]})))
    (component ".advance"
      (assertions
        "advances :all of the tapes"
        (-> irobot (assoc :tapes [[0 1] [2 3]])
            (.advance :all))
        =fn=> (*contains? {:tapes [[1] [3]]})
        "advances only a specific tape"
        (-> irobot (assoc :tapes [[0 1] [2 3]])
            (.advance 0))
        =fn=> (*contains? {:tapes [[1] [2 3]]})

        "logs the tape heads before advancing"
        (-> irobot (assoc :tapes [[0 1] [2 3]]) (.advance :all))
        =fn=> (*contains? {:history [[1 3]]})))
    (component ".accept"
      (assertions
        "sets the state to :accepted"
        (-> irobot (.accept))
        =fn=> (*contains? {:state :accepted})
        "logs the state transition"
        (-> irobot (assoc :tapes [[0] [1]]) (.accept))
        =fn=> (*contains? {:history ["->accepted"]})))
    (component ".reject"
      (assertions "sets the state to rejected"
        (-> irobot (.reject nil))
        =fn=> (*contains? {:state :rejected})
        "sets :rejected/msg to the passed msg"
        (-> irobot (.reject "ipsum"))
        =fn=> (*contains? {:rejected/msg "ipsum"})
        "logs the transition"
        (-> irobot (assoc :tapes [[0] [1]]) (.reject "foo"))
        =fn=> (*contains? {:history ["->rejected"]})))
    (component ".value"
      (assertions
        "or sets :value to a val"
        (-> irobot (.value 0))
        =fn=> (*contains? {:value 0})
        "updates :value with a fn"
        (-> irobot (.value 0) (.value inc))
        =fn=> (*contains? {:value 1}))))
  (component "build"
    (let [auto (build {:k1 :v1}
                      {:state1 :step1})]
      (assertions
        "is a Machine & implements IRobot"
        auto =fn=> #(instance? machine.core.Machine %)
        auto =fn=> machine?
        auto =fn=> #(instance? machine.core.IRobot %)
        (keys auto) =fn=> (*contains? [:value :state :tapes :steps])

        "has initial state of :normal"
        auto =fn=> (*contains? {:state :normal})
        "takes an initial value"
        auto =fn=> (*contains? {:value {:k1 :v1}})
        "takes a mapping of state to step-fn"
        auto =fn=> (*contains? {:steps {:state1 :step1}})
        "tapes are in a vector"
        (:tapes auto) =fn=> vector?
        "can take k-v :accept-states"
        (build {:accept-states #{:foo}})
        =fn=> (*contains? {:accept-states #{:foo}})
        "accept-states defaults to all the step keys"
        auto =fn=> (*contains? {:accept-states #{:state1}})
        "can take on-empty"
        (build {:on-empty println})
        =fn=> (*contains? {:on-empty println}))))

  (def only-even-auto
    (build {} {:normal (fn [{:keys [tapes] :as auto}]
                         (if (even? (ffirst tapes))
                           (.advance auto :all)
                           (.reject auto (ffirst tapes))))}))

  (def ^:dynamic *just-log* false)
  (def logging-auto
    (build {:saw []}
           {:normal (fn [{:keys [value tapes] :as auto}]
                      (-> auto
                          (update-in [:value :saw] conj
                                     (ffirst tapes))
                          (#(if-not *just-log*
                              (.advance % :all) %))))}))

  (def even-odd-auto
    (build {:normal #(let [new-state (if (even? (ffirst (:tapes %)))
                                       :even :odd)]
                       (-> % (.transition new-state)
                           (assoc-in [:value] new-state)
                           (.advance :all)))
            :even #(if (even? (ffirst (:tapes %)))
                     (.advance % :all)
                     (.reject % (ffirst (:tapes %))))
            :odd #(if (odd? (ffirst (:tapes %)))
                    (.advance % :all)
                    (.reject % (ffirst (:tapes %))))}))

  (def only-small
    (build {:normal #(-> % (.transition (if (< (ffirst (:tapes %)) 10)
                                          :small :big))
                         (.advance :all))
            :small #(if (< (ffirst (:tapes %)) 10)
                      (.advance % :all)
                      (.transition % :big))
            :big #(.advance % :all)
            :accept-states #{:small}}))

  (component "run"
    (behavior "executes appr step fn, dflt is :normal"
      (assertions "accepts if ran to completion & accepting state"
        (run logging-auto [0 1 2])
        =fn=> (*contains? {:value {:saw [0 1 2]}
                           :history [[0] [1] [2]]
                           :state :accepted})
        (run only-even-auto [0 2 4])
        =fn=> (*contains? {:state :accepted})
        (run only-small (range 10))
        =fn=> (*contains? {:state :accepted})
        (run only-small [9 10 11])
        =fn=> (*contains? {:state :rejected}))
      (behavior "executes on-empty if any tape ran out"
        (let [called (atom 0)]
          (t/is (= 1 (do (run (build {:normal #(.advance % :all)
                                      :on-empty (fn [& _] (swap! called inc))})
                              [0 1] [0])
                         @called)))))
      (assertions
        "using its return value if its a machine"
        (run (build {:normal #(.advance % :all)
                     :on-empty (fn [a fst sec]
                                 (-> a (.value (or fst sec))
                                     (.accept)))})
             [0 1] [2])
        =fn=> (*contains?  {:value '(1)
                            :state :accepted})
        "only if not all are empty"
        (run (build 0 {:normal #(.advance % :all)
                       :on-empty (fn [a & _]
                                   (.value a inc))})
             [0 1] [2 3])
        =fn=> (*contains?  {:value 0})
        "rejecting if reject was called"
        (run only-even-auto [0 1 2])
        =fn=> (*contains?  {:state :rejected
                            :rejected/msg 1})
        "step fn's can transition to other states"
        (run even-odd-auto [0 1 2])
        =fn=> (*contains? {:state :rejected
                           :value :even
                           :history [[0] "->even" [1] "->rejected"]
                           :rejected/msg 1})
        (run even-odd-auto [1 2 3])
        =fn=> (*contains?
                {:state :rejected
                 :value :odd
                 :rejected/msg 2}))))

  (component "compose"
    (assertions "composes r->l the step fns & merges initial values"
      (binding [*just-log* true]
        (run (compose only-even-auto logging-auto) [0 2 5 13]))
      =fn=> (*contains?
              {:state :rejected
               :value {:saw [0 2 5]}
               :history [[0] [2] [5] "->rejected"]
               :rejected/msg 5}))))
