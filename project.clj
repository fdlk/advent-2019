(defproject advent-2019 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [
                 [org.clojure/clojure "1.10.0"]
                 [criterium "0.4.5"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/data.priority-map "0.0.10"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :main ^:skip-aot advent-2019.day18
  :target-path "target/%s"
  :plugins [[lein-cljfmt "0.6.6"]]
  ; :jvm-opts ["-Xmx16g"]
  :profiles {:uberjar {:aot :all}})
