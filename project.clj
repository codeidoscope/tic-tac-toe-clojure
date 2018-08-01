(defproject tic-tac-toe-clojure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]]

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "0.0-3308"]
                                  [speclj "3.3.1"]]}}
  :plugins [[speclj "3.3.1"]]

  ["src/clj"]
  :test-paths ["spec/clj"]
