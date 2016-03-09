(defproject machine "1.0.2"
  :description "Library for reading multiple input tapes with a concept of states borrowed from Finite State Machines."
  :url "https://github.com/Pancia/machine"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [navis/untangled-spec "0.3.5" :scope "test"]]
  :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]
  :test-refresh {:changes-only true
                 :with-repl true
                 :report untangled-spec.reporters.terminal/untangled-report}
  )
