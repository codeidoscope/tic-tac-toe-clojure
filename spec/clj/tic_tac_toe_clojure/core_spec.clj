(ns tic-tac-toe-clojure.core-spec
  (:require [speclj.core :refer :all]
            [tic-tac-toe-clojure.core :refer :all]))

(describe "A board"
  (it "has 9 cells"
    (should= 
      [nil nil nil nil nil nil nil nil nil]
      (create-board))))
