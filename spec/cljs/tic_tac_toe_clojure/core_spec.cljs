(ns tic-tac-toe-clojure.core-spec 
  (:require-macros [speclj.core :refer [describe it should=]])
  (:require [speclj.core]
            [tic-tac-toe-clojure.core]))

(describe "A ClojureScript test"
  (it "fails. Fix it!"
    (should= 0 1)))
