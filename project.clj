(defproject tic-tac-toe-clojure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :main tic-tac-toe-clojure.core

  :dependencies [[org.clojure/clojure "1.9.0"]]

  :profiles {:dev {:dependencies [[speclj "3.3.2"]]}}

  :plugins [[speclj "3.3.2"]]

  :source-paths ["src/clj"]
  :test-paths ["spec/clj"])
