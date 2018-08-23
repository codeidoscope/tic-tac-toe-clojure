(ns tic-tac-toe-clojure.core-spec (:require [speclj.core :refer :all] [tic-tac-toe-clojure.core :refer :all]))

(describe "A board"
  (it "has 9 cells"
      (should=
        ["_" "_" "_" "_" "_" "_" "_" "_" "_"]
        (create-board)))

  (it "sets a X mark at the correct position"
      (should=
        ["X" "_" "_" "_" "_" "_" "_" "_" "_"]
        (set-position (create-board) 0 "X")))

  (it "sets a X mark O mark at the correct position"
      (should=
        ["X" "O" "_" "_" "_" "_" "_" "_" "_"]
        (set-position (set-position (create-board) 0 "X") 1 "O")))

  (it "sets a O mark at the correct position"
      (should=
        ["O" "_" "_" "_" "_" "_" "_" "_" "_"]
        (set-position (create-board) 0 "O")))

  (it "is displayed on three lines"
      (should=
        "___\n___\n___\n"
        (build-board (create-board)))

      (should=
        "012\n345\n678\n"
        (build-board numbered-board))))

(describe "A game"
          (with-stubs)
  (it "prompts a player to pick a symbol"
      (should=
        "Y"
       (with-in-str "Y" (get-player-symbol))))

  (it "prompts a human player to pick a position on the board"
      (should=
       1
        (with-in-str "1" (get-human-position))))

  (it "prompts a player to select a player type"
      (should=
        "C"
        (with-in-str "C" (get-player-type))))

  (it "picks a random empty position for a computer"
      (should=
        3
        (get-computer-position ["X" "_" "O" "_" "X" "_" "X" "_" "O"] (fn [_] [3 "O"]))))

  (it "gets the player position if player is human"
      (should=
        1
        (with-in-str "1" (get-player-position ["X" "_" "O" "_" "X" "_" "X" "_" "O"] "h"))))
  (it "swaps a player's symbol"
      (should=
        "X"
        (swap-player "O")))

  (it "tells the player the game is over"
      (should=
        "Game is over"
        end-game)))

(describe "A decision engine"
  (it "gets the rows from a board"
      (should=
        [["X" "_" "_"] ["O" "_" "_"] ["X" "_" "_"]]
        (get-rows (set-position (set-position (set-position (create-board) 0 "X") 3 "O") 6 "X"))))

  (it "gets the columns from a board"
      (should=
        [["X" "_" "_"] ["O" "_" "_"] ["X" "_" "_"]]
        (get-columns (set-position (set-position (set-position (create-board) 0 "X") 1 "O") 2 "X"))))

  (it "gets the diagonals from a board"
      (should=
        [["X" "O" "X"] ["O" "O" "O"]]
        (get-diagonals (set-position (set-position (set-position (set-position (set-position (create-board) 0 "X") 4 "O") 8 "X") 2 "O") 6 "O"))))

  (it "joins the rows, columns and diagonals"
     (should=
       [["X" "_" "O"] ["_" "X" "_"] ["O" "_" "X"] ["X" "_" "O"] ["_" "X" "_"] ["O" "_" "X"] ["X" "X" "X"] ["O" "X" "O"]]
       (join-sections (set-position (set-position (set-position (set-position (set-position (create-board) 0 "X") 4 "X") 8 "X") 2 "O") 6 "O"))))

  (it "checks if three symbols are aligned"
      (should=
        true
        (three-mapped? (set-position (set-position (set-position (set-position (set-position (create-board) 0 "X") 4 "X") 8 "X") 2 "O") 6 "O"))))

  (it "returns true if a board is full"
      (should=
        true
        (board-full? (set-position (set-position (set-position (set-position (set-position (set-position (set-position (set-position (set-position (create-board) 0 "X") 1 "O") 2 "X") 3 "O") 4 "X") 5 "O") 6 "X") 7 "O") 8 "X"))))

  (it "returns false if a board is not full"
      (should=
        false
        (board-full? (set-position (set-position (set-position (set-position (set-position (set-position (set-position (set-position (set-position (create-board) 0 "X") 1 "O") 2 "X") 3 "O") 4 "X") 5 "O") 6 "X") 7 "O") 8 "_"))))

  (it "returns true if the game is over because three symbols are aligned"
      (should=
        true
        (game-over? (set-position (set-position (set-position (set-position (set-position (create-board) 0 "X") 4 "X") 8 "X") 2 "O") 6 "O"))))

  (it "returns true if the game is over because the board is full"
      (should=
        true
        (game-over? ["X" "O" "X" "O" "X" "O" "X" "O" "X"]))))
