(defproject db "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/java.jdbc "0.6.2-alpha3"]
                 [org.xerial/sqlite-jdbc "3.7.2"]
                 [korma "0.4.3"]]
  :main ^:skip-aot db.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
