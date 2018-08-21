(ns tic-tac-toe-clojure.core-spec (:require [speclj.core :refer :all] [tic-tac-toe-clojure.core :refer :all]))

(describe "A board"
  (it "has 9 cells"
      (should=
        ["_" "_" "_" 
         "_" "_" "_"
         "_" "_" "_"]
        (create-board)))

  (it "sets a X mark at the correct position"
      (should=
        ["X" "_" "_" 
         "_" "_" "_" 
         "_" "_" "_"]
        (set-position (create-board) 0 "X")))

  (it "sets an X mark followed by an O mark in the correct positions"
      (should=
        ["X" "O" "_" 
         "_" "_" "_" 
         "_" "_" "_"]
        (set-position (set-position (create-board) 0 "X") 1 "O")))

  (it "sets an O mark in the correct position"
      (should=
        ["O" "_" "_" 
         "_" "_" "_" 
         "_" "_" "_"]
        (set-position (create-board) 0 "O")))

  (it "is displayed on three lines"
      (should=
        "___\n___\n___\n"
        (format-board (create-board)))

      (should=
        "012\n345\n678\n"
        (format-board numbered-board))))

(describe "A game"
  (it "prompts a user to pick a symbol"
      (should=
        "Y"
       (with-in-str "Y" (get-user-symbol))))

  (it "prompts a user to pick a position on the board"
      (should=
       1
        (with-in-str "1" (get-user-position))))

  (it "swaps a player's symbol"
      (should=
        "X"
        (swap-player "O")))

  (it "tells the user the game is over"
      (should=
        "Game is over"
        end-game)))

(describe "A decision engine"
  (it "gets the rows from a board"
      (should=
        [["X" "_" "_"] ["O" "_" "_"] ["X" "_" "_"]]
        (get-rows ["X" "_" "_" 
                   "O" "_" "_" 
                   "X" "_" "_"])))

  (it "gets the columns from a board"
      (should=
        [["X" "_" "_"] ["O" "_" "_"] ["X" "_" "_"]]
        (get-columns ["X" "O" "X" 
                      "_" "_" "_" 
                      "_" "_" "_"])))

  (it "gets the diagonals from a board"
      (should=
        [["X" "O" "X"] ["O" "O" "O"]]
        (get-diagonals ["X" "_" "O" 
                        "_" "O" "_" 
                        "O" "_" "X"])))

  (it "joins the rows, columns and diagonals"
     (should=
       [["X" "_" "O"] ["_" "X" "_"] ["O" "_" "X"]
        ["X" "_" "O"] ["_" "X" "_"] ["O" "_" "X"] 
        ["X" "X" "X"] ["O" "X" "O"]]
       (join-sections ["X" "_" "O"
                       "_" "X" "_" 
                       "O" "_" "X"])))

  (it "checks if three symbols are aligned"
      (should=
        true
        (three-aligned? ["X" "_" "O" 
                         "_" "X" "_" 
                         "O" "_" "X"])))

  (it "returns true if a board is full"
      (should=
        true
        (board-full? ["X" "O" "X" 
                      "O" "X" "O" 
                      "X" "O" "X"])))

  (it "returns false if a board is not full"
      (should=
        false
        (board-full? ["X" "O" "X" 
                      "O" "X" "O" 
                      "X" "O" "_"])))

  (it "returns true if the game is over because three symbols are aligned"
      (should=
        true
        (game-over? (set-position (set-position (set-position (set-position (set-position (create-board) 0 "X") 4 "X") 8 "X") 2 "O") 6 "O"))))

  (it "returns true if the game is over because the board is full"
      (should=
        true
        (game-over? ["X" "O" "X" 
                     "O" "X" "O" 
                     "X" "O" "X"]))))
