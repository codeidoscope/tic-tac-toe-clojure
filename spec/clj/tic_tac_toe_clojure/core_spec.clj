(ns tic-tac-toe-clojure.core-spec
  (:require [speclj.core :refer :all]
            [tic-tac-toe-clojure.core :refer :all]))

(describe "A board"
  (it "has 9 cells"
    (should= 
      ["_" "_" "_" "_" "_" "_" "_" "_" "_"]
      create-board))
  (it "is displayed on three lines"
       (should=
         "___\n___\n___\n"
         (display-board create-board)))
  (it "shows numbered cells"
      (should=
        "123\n456\n789\n"
        (display-board numbered-board))))
