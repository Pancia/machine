(ns machine.core-spec
  (:require [machine.core :refer [build run compose machine?]]
            [clojure.test :as t]
            [clojure.set :as set]))

(defn *contains? [act-map exp-map]
  (set/subset? (set exp-map) (set act-map)))

(def irobot (build {}))

(t/deftest MachineCore
  (t/testing "IRobot"
    (t/testing ".log"
      (t/testing "adds a msg to :history"
        (t/is (*contains? (-> irobot (.log 0))
                          {:history [0]}))
        (t/is (*contains? (-> irobot (.log 0) (.log 1))
                          {:history [0 1]})))
      (t/testing "logs using a fn that takes the auto"
        (t/is (*contains? (-> irobot (.log #(->> % type str (re-find #"\w+$"))))
                          {:history ["Machine"]}))))
    (t/testing ".transition"
      (t/testing "updates :state using a fn"
        (t/is (*contains? (.transition irobot #(->> % name (str "new-") keyword))
                          {:state :new-normal})))
      (t/testing "or sets :state to a val"
        (t/is (*contains? (.transition irobot :new-state)
                          {:state :new-state})))
      (t/testing "logs the transition"
        (t/is (*contains? (.transition irobot :blah)
                          {:history ["->blah"]}))))
    (t/testing ".advance"
      (t/testing "advances :all of the tapes"
        (t/is (*contains? (-> irobot (assoc :tapes [[0 1] [2 3]])
                              (.advance :all))
                          {:tapes [[1] [3]]})))
      (t/testing "advances only a specific tape"
        (t/is (*contains? (-> irobot (assoc :tapes [[0 1] [2 3]])
                              (.advance 0))
                          {:tapes [[1] [2 3]]})))

      (t/testing "logs the tape heads before advancing"
        (t/is (*contains? (-> irobot (assoc :tapes [[0 1] [2 3]]) (.advance :all))
                          {:history [[0 2]]}))))
    (t/testing ".accept"
      (t/testing "sets the state to :accepted"
        (t/is (*contains? (-> irobot (.accept))
                          {:state :accepted})))
      (t/testing "logs the tape heads"
        (t/is (*contains? (-> irobot (assoc :tapes [[0] [1]]) (.accept))
                          {:history [[0 1] "->accepted"]}))))
    (t/testing ".reject"
      (t/testing "sets the state to rejected"
        (t/is (*contains? (-> irobot (.reject nil))
                          {:state :rejected})))
      (t/testing "sets :rejected/msg to the passed msg"
        (t/is (*contains? (-> irobot (.reject "ipsum"))
                          {:rejected/msg "ipsum"})))
      (t/testing "logs the tape heads"
        (t/is (*contains? (-> irobot (assoc :tapes [[0] [1]]) (.reject "foo"))
                          {:history [[0 1] "->rejected"]}))))
    (t/testing ".value"
      (t/testing "or sets :value to a val"
        (t/is (*contains? (-> irobot (.value 0))
                          {:value 0})))
      (t/testing "updates :value with a fn"
        (t/is (*contains? (-> irobot (.value 0) (.value inc))
                          {:value 1})))))
  (t/testing "build"
    (let [auto (build {:k1 :v1}
                      {:state1 :step1})]
      (t/testing "is a Machine & implements IRobot"
        (t/is (instance? machine.core.Machine auto))
        (t/is (machine? auto))
        (t/is (*contains? (keys auto)
                          [:value :state :tapes :steps]))
        (t/is (instance? machine.core.IRobot auto)))
      (t/testing "has initial state of :normal"
        (t/is (*contains? auto {:state :normal})))
      (t/testing "takes an initial value"
        (t/is (*contains? auto {:value {:k1 :v1}})))
      (t/testing "takes a mapping of state to step-fn"
        (t/is (*contains? auto {:steps {:state1 :step1}})))
      (t/testing "tapes are in a vector"
        (t/is (vector? (:tapes auto))))
      (t/testing "can take k-v :accept-states"
        (t/is (*contains? (build {:accept-states #{:foo}})
                          {:accept-states #{:foo}})))
      (t/testing "accept-states defaults to all the step keys"
        (t/is (*contains? auto
                          {:accept-states #{:state1}})))
      (t/testing "can take on-empty"
        (t/is (*contains? (build {:on-empty println})
                          {:on-empty println})))))

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

  (t/testing "run"
    (t/testing "executes appr step fn, dflt is :normal"
      (t/testing "accepts if ran to completion & accepting state"
        (t/is (*contains? (run logging-auto [0 1 2])
                          {:value {:saw [0 1 2]}
                           :history [[0] [1] [2]]
                           :state :accepted}))
        (t/is (*contains? (run only-even-auto [0 2 4])
                          {:state :accepted}))
        (t/is (*contains? (run only-small (range 10))
                          {:state :accepted}))
        (t/is (*contains? (run only-small [9 10 11])
                          {:state :rejected})))
      (t/testing "executes on-empty if any tape ran out"
        (let [called (atom 0)]
          (t/is (= 1 (do (run (build {:normal #(.advance % :all)
                                      :on-empty (fn [& _] (swap! called inc))})
                              [0 1] [0])
                         @called))))
        (t/testing "using its return value if its a machine"
          (t/is (*contains? (run (build {:normal #(.advance % :all)
                                         :on-empty (fn [a fst sec]
                                                     (-> a (.value (or fst sec))
                                                         (.accept)))})
                                 [0 1] [2])
                            {:value '(1)
                             :state :accepted})))
        (t/testing "only if not all are empty"
          (t/is (*contains? (run (build 0 {:normal #(.advance % :all)
                                           :on-empty (fn [a & _]
                                                       (.value a inc))})
                                 [0 1] [2 3])
                            {:value 0}))))
      (t/testing "rejecting if reject was called"
        (t/is (*contains? (run only-even-auto [0 1 2])
                          {:state :rejected
                           :rejected/msg 1})))
      (t/testing "step fn's can transition to other states"
        (t/is (*contains? (run even-odd-auto [0 1 2])
                          {:state :rejected
                           :value :even
                           :history ["->even" [0] [1] "->rejected"]
                           :rejected/msg 1}))
        (t/is (*contains? (run even-odd-auto [1 2 3])
                          {:state :rejected
                           :value :odd
                           :rejected/msg 2})))))

  (t/testing "compose"
    (t/testing "composes r->l the step fns & merges initial values"
      (t/is (*contains? (binding [*just-log* true]
                          (run (compose only-even-auto logging-auto) [0 2 5 13]))
                        {:state :rejected
                         :value {:saw [0 2 5]}
                         :history [[0] [2] [5] "->rejected"]
                         :rejected/msg 5})))))
