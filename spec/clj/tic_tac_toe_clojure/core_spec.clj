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

  (it "tests a Computer VS Human game"
      (let [output (with-out-str (with-in-str "c\nX\n2\n5\n8" (start-game)))]
        (should= "Please select an opponent (H for human or C for computer): c\nPlease choose a symbol (X or O): 012\n345\n678\n___\n___\n___\nX__\n___\n___\nPlease choose a position between 0 and 8: X_O\n___\n___\nXXO\n___\n___\nPlease choose a position between 0 and 8: XXO\n__O\n___\nXXO\nX_O\n___\nPlease choose a position between 0 and 8: XXO\nX_O\n__O\nGame is over\n"
                 output)))
  
  (it "is displayed on three lines with separators when empty"
      (should=
        "_ | _ | _\n---------\n_ | _ | _\n---------\n_ | _ | _\n---------\n"
        (format-board (create-board))))

  (it "is displayed on three lines with separators when numbered"
      (should=
        "0 | 1 | 2\n---------\n3 | 4 | 5\n---------\n6 | 7 | 8\n---------\n"
        (format-board numbered-board))))

(describe "A game"
;          (with-stubs)
  (it "prompts a player to pick a symbol"
      (should=
        "Y"
       (with-in-str "Y" (prompt-user "fake prompt"))))

  (it "prompts a human player to pick a position on the board"
      (should=
       1
        (with-in-str "1" (get-human-position))))

  (it "prompts a player to select a player type"
      (should=
        "C"
        (with-in-str "C" (get-player-type))))

;  (it "picks a random empty position for a computer"
;      (should=
;        3
;        (get-computer-position ["X" "_" "O" "_" "X" "_" "X" "_" "O"] (fn [_] [3 "O"]))))

  (it "picks the first available position for a computer"
      (should=
        1
        (get-first-available-position ["X" "_" "O" "_" "X" "_" "X" "_" "O"])))

  (it "gets the player position if player is human"
      (should=
        1
        (with-in-str "1" (get-player-position ["X" "_" "O" "_" "X" "_" "X" "_" "O"] "h"))))

  ;(it "gets the player position if player is computer"
  ;    (with-redefs 
  ;      [get-computer-position (stub :get-computer-position)
  ;                  (get-player-position ["X" "_" "O" "_" "X" "_" "X" "_" "O"] "c")
  ;                  (should-have-invoked :get-computer-position {:with [:board :randomiser]})]))

   (it "swaps a human player's type"
       (should=
         "c"
         (swap-player-type "c" "h")))

   (it "swaps a computer player's type"
       (should=
         "h"
         (swap-player-type "c" "c")))

  (it "swaps a player's nought symbol"
      (should=
        "X"
        (swap-player-symbol "O")))

  (it "swaps a player's cross symbol"
     (should=
       "X"
       (swap-player-symbol "O"))))

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
