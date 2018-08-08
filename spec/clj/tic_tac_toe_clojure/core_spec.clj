(ns tic-tac-toe-clojure.core-spec (:require [speclj.core :refer :all] [tic-tac-toe-clojure.core :refer :all]))

(describe "A board"
  (it "has 9 cells"
    (should=
      ["_" "_" "_" "_" "_" "_" "_" "_" "_"]
      create-board))

  (it "sets a X mark at the correct position"
    (should=
      ["X" "_" "_" "_" "_" "_" "_" "_" "_"]
      (set-position create-board 0 "X")))

  (it "sets a X mark O mark at the correct position"
    (should=
      ["X" "O" "_" "_" "_" "_" "_" "_" "_"]
      (set-position (set-position create-board 0 "X") 1 "O")))

  (it "sets a O mark at the correct position"
    (should=
      ["O" "_" "_" "_" "_" "_" "_" "_" "_"]
      (set-position create-board 0 "O")))

  (it "is displayed on three lines"
       (should=
         "___\n___\n___"
         (display-board create-board))
         "___\n___\n___\n"
      (should=
        "123\n456\n789"
        (display-board numbered-board))))

(describe "A game"
  (it "prompts a user to pick a symbol"
      (should-contain
        "Y"
       (with-in-str "Y" (get-user-symbol))))

  (it "prompts a user to pick a position on the board"
      (should-contain
       "1"
        (with-in-str "1" (get-user-position))))

  (it "swaps a player's symbol"
      (should=
        "X"
        (swap-player "O"))))
