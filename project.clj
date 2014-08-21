(defproject clj-65xx "0.1.0-SNAPSHOT"
  :description "65xx chip emulator/assembler"
  :url "https://github.com/wizzomafizzo/clj-65xx"
  :license {:name "MIT"
            :url "https://github.com/wizzomafizzo/clj-65xx/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.trace "0.7.8"]]
  :main ^:skip-aot clj-65xx.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :global-vars {*print-length* 100})
