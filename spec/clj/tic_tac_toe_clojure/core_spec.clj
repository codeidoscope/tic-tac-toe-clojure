(ns tic-tac-toe-clojure.core-spec
  (:require [speclj.core :refer :all]
            [tic-tac-toe-clojure.core :refer :all]))

(defn all-computer-boards [board human-player computer-player]
  (mapcat identity
    (for [spot (empty-spots board)]
      (let [board-with-human-move (set-position board spot human-player)]
        (if (game-over? board-with-human-move human-player computer-player)
          [board-with-human-move]
          (let [computer-move (choose-best-move board-with-human-move computer-player human-player)
                board-with-computer-move (set-position board-with-human-move computer-move computer-player)]
            (if (game-over? board-with-computer-move computer-player human-player)
              [board-with-human-move board-with-computer-move]
              (concat [board-with-human-move board-with-computer-move]
                (all-computer-boards board-with-computer-move human-player computer-player)))))))))

(describe "A board"
  (it "has 9 cells"
    (should= ["_" "_" "_"
              "_" "_" "_"
              "_" "_" "_"]
      (create-board)))

  (it "sets a X mark at the correct position"
    (should= ["X" "_" "_"
              "_" "_" "_"
              "_" "_" "_"]
      (set-position (create-board) 0 "X")))

  (it "sets an X mark followed by an O mark in the correct positions"
    (should= ["X" "O" "_"
              "_" "_" "_"
              "_" "_" "_"]
      (set-position (set-position (create-board) 0 "X") 1 "O")))

  (it "sets an O mark in the correct position"
      (should=
        ["O" "_" "_"
         "_" "_" "_"
         "_" "_" "_"]
        (set-position (create-board) 0 "O")))

  (it "is displayed on three lines with separators when empty"
      (should=
        "_ | _ | _\n---------\n_ | _ | _\n---------\n_ | _ | _\n---------\n"
        (format-board (create-board))))

  (it "is displayed on three lines with separators when numbered"
      (should=
        "0 | 1 | 2\n---------\n3 | 4 | 5\n---------\n6 | 7 | 8\n---------\n"
        (format-board numbered-board))))

(describe "A UI"
  (it "prompts a player to pick a symbol"
    (should= "Y"
      (with-in-str "Y" (prompt-user "Fake prompt "))))

  (it "prompts a human player to pick a position on the board"
    (should= 1
      (with-in-str "1" (get-human-position)))))

(describe "A decision engine"
  (it "gets the rows from a board"
    (should= [["X" "_" "_"] ["O" "_" "_"] ["X" "_" "_"]]
      (get-rows ["X" "_" "_"
                 "O" "_" "_"
                 "X" "_" "_"])))

  (it "gets the columns from a board"
    (should= [["X" "_" "_"] ["O" "_" "_"] ["X" "_" "_"]]
      (get-columns ["X" "O" "X"
                    "_" "_" "_"
                    "_" "_" "_"])))

  (it "gets the diagonals from a board"
      (should= [["X" "O" "X"] ["O" "O" "O"]]
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
    (should= true
      (three-aligned? ["X" "_" "O"
                       "_" "X" "_"
                       "O" "_" "X"] "X")))

  (it "returns true if a board is full"
    (should= true
      (board-full? ["X" "O" "X"
                    "O" "X" "O"
                    "X" "O" "X"])))

  (it "returns false if a board is not full"
    (should= false
      (board-full? ["X" "O" "X"
                    "O" "X" "O"
                    "X" "O" "_"])))

  (it "returns true if the game is over because three symbols are aligned"
    (should= true
      (game-over? (set-position (set-position (set-position (set-position (set-position (create-board) 0 "X") 4 "X") 8 "X") 2 "O") 6 "O") "X" "O")))

  (it "returns true if the game is over because the board is full"
    (should= true
      (game-over? ["X" "O" "X"
                   "O" "X" "O"
                   "X" "O" "X"] "X" "O"))))

(describe "A game"
  (it "tests a Human VS Human game"
      (let [output (with-out-str (with-in-str "h\nh\n0\n2\n3\n5\n6" (start-game)))
            board-state-1 (create-board)
            board-state-2 (set-position board-state-1 0 "O")
            board-state-3 (set-position board-state-2 2 "X")
            board-state-4 (set-position board-state-3 3 "O")
            board-state-5 (set-position board-state-4 5 "X")
            board-state-6 (set-position board-state-5 6 "O")]
        (should=
          (str select-opponent
               select-opponent
               (format-board numbered-board)
               (format-board board-state-1)
               select-position
               (format-board board-state-2)
               select-position
               (format-board board-state-3)
               select-position
               (format-board board-state-4)
               select-position
               (format-board board-state-5)
               select-position
               (format-board board-state-6)
               end-game"\n")
          output)))

    (it "tests a Human VS Computer game"
        (let [output (with-out-str (with-in-str "h\nc\n2\n5\n8" (start-game)))
              board-state-1 (create-board)
              board-state-2 (set-position board-state-1 0 "O")
              board-state-3 (set-position board-state-2 2 "X")
              board-state-4 (set-position board-state-3 1 "O")
              board-state-5 (set-position board-state-4 5 "X")
              board-state-6 (set-position board-state-5 3 "O")
              board-state-7 (set-position board-state-6 8 "X")]
          (should=
            (str select-opponent
                 select-opponent
                 (format-board numbered-board)
                 (format-board board-state-1)
                 (format-board board-state-2)
                 select-position
                 (format-board board-state-3)
                 (format-board board-state-4)
                 select-position
                 (format-board board-state-5)
                 (format-board board-state-6)
                 select-position
                 (format-board board-state-7)
                 end-game"\n")
            output)))

    (it "tests a Computer VS Human game"
        (let [output (with-out-str (with-in-str "c\nh\n2\n5\n8" (start-game)))
              board-state-1 (create-board)
              board-state-2 (set-position board-state-1 2 "O")
              board-state-3 (set-position board-state-2 0 "X")
              board-state-4 (set-position board-state-3 5 "O")
              board-state-5 (set-position board-state-4 1 "X")
              board-state-6 (set-position board-state-5 8 "O")]
          (should=
            (str select-opponent
                 select-opponent
                 (format-board numbered-board)
                 (format-board board-state-1)
                 select-position
                 (format-board board-state-2)
                 (format-board board-state-3)
                 select-position
                 (format-board board-state-4)
                 (format-board board-state-5)
                 select-position
                 (format-board board-state-6)
                 end-game"\n")
            output)))

    (it "tests a Computer VS Computer game"
        (let [output (with-out-str (with-in-str "c\nc\n" (start-game)))
              board-state-1 (create-board)
              board-state-2 (set-position board-state-1 0 "O")
              board-state-3 (set-position board-state-2 1 "X")
              board-state-4 (set-position board-state-3 2 "O")
              board-state-5 (set-position board-state-4 3 "X")
              board-state-6 (set-position board-state-5 4 "O")
              board-state-7 (set-position board-state-6 5 "X")
              board-state-8 (set-position board-state-7 6 "O")]
          (should=
            (str select-opponent
                 select-opponent
                 (format-board numbered-board)
                 (format-board board-state-1)
                 (format-board board-state-2)
                 (format-board board-state-3)
                 (format-board board-state-4)
                 (format-board board-state-5)
                 (format-board board-state-6)
                 (format-board board-state-7)
                 (format-board board-state-8)
                 end-game"\n")
            output))))

(describe "Minimax"
  (it "returns a winning position for player X on the board"
    (should= 8
      (get-computer-position ["O" "_" "X"
                              "O" "_" "X"
                              "X" "_" "_"] "X" "O")))

  (it "returns a winning position for player X when there are three spots left"
    (should= 6
      (get-computer-position ["O" "X" "_"
                              "O" "O" "_"
                              "_" "X" "X"] "X" "O")))

  (it "returns a position and positive score when a winning position is available"
    (should= 8
      (get-computer-position ["O" "X" "X"
                              "O" "O" "X"
                              "X" "O" "_"] "X" "O")))

  (it "returns a position and negative score when the opponent is in a winning position"
    (should= 8
      (get-computer-position ["O" "X" "O"
                              "O" "X" "X"
                              "O" "O" "_"] "X" "O")))

  (it "returns a position and 0 when the game is a draw"
    (should= 8
      (get-computer-position ["O" "X" "O"
                              "O" "X" "X"
                              "X" "O" "_"] "X" "O")))

  (it "returns a score of 10 if the game is a win"
    (should= 10
      (evaluate-board ["X" "_" "_"
                       "X" "O" "O"
                       "X" "_" "_"] "X" "O")))

  (it "returns a score of -10 if the game is a loss"
    (should= -10
      (evaluate-board ["X" "_" "_"
                       "O" "O" "O"
                       "X" "_" "_"] "X" "O")))

  (it "returns a score of zero if the game is a draw"
    (should= 0
      (evaluate-board ["X" "X" "O"
                       "O" "O" "X"
                       "X" "O" "_"] "X" "O")))

  (it "returns a hash that includes the position and the score of the move when there is a winning move"
    (should= {5 10}
      (score-move ["O" "X" "X"
                   "O" "_" "_"
                   "_" "O" "X"] 5 "X" "O")))

  (it "returns a hash that includes the position and the score of the move when there is a neutral move"
    (should= {4 0}
      (score-move ["X" "O" "X"
                   "O" "_" "_"
                   "_" "X" "O"] 4 "X" "O")))

  (it "returns a hash that includes the position and the score of the move when there is a losing move"
    (should= {4 -10}
      (score-move ["O" "X" "X"
                   "O" "_" "_"
                   "O" "O" "X"] 4 "X" "O")))

  (it "returns a list of scored positions in hashes"
    (should= {3 0, 4 -10, 5 10}
      (scored-moves ["O" "X" "X"
                     "_" "_" "_"
                     "O" "O" "X"] "X" "O")))

  (it "returns an empty list if the board is full"
    (should= {}
      (scored-moves ["O" "X" "X"
                     "X" "O" "O"
                     "O" "O" "X"] "X" "O")))

  (it "returns a winning position for the maximising player"
    (should= 5
      (choose-best-move ["O" "X" "X"
                         "_" "_" "_"
                         "O" "O" "X"] "X" "O")))

  (it "returns a position that will make the maximising player lose"
    (should= 3
      (choose-best-move ["O" "X" "X"
                         "_" "_" "_"
                         "O" "O" "X"] "O" "X")))

  (it "returns a position that will make the maximising player lose"
    (should= 5
      (choose-best-move ["O" "X" "X"
                         "X" "_" "_"
                         "_" "O" "X"] "O" "X")))

  (it "does not loose"
     (doseq [board (all-computer-boards (create-board) "X" "0")]
       (should (>= 0 (evaluate-board board "O" "X"))))))
