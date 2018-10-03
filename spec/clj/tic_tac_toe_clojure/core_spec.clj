(ns tic-tac-toe-clojure.core-spec
  (:require [speclj.core :refer :all]
            [tic-tac-toe-clojure.core :refer :all]))

(defn all-computer-boards [board human-player computer-player]
  (mapcat identity
    (for [spot (get-empty-spots board)]
      (let [board-with-human-move (set-position board spot human-player)]
        (if (game-over? board-with-human-move human-player computer-player 3)
          [board-with-human-move]
          (let [computer-move (choose-best-move board-with-human-move computer-player human-player 0 3)
                board-with-computer-move (set-position board-with-human-move computer-move computer-player)]
            (if (game-over? board-with-computer-move computer-player human-player 3)
              [board-with-human-move board-with-computer-move]
              (concat [board-with-human-move board-with-computer-move]
                (all-computer-boards board-with-computer-move human-player computer-player)))))))))

(describe "A board"
  (it "has 9 cells for a 3x3 board"
    (should= ["_" "_" "_"
              "_" "_" "_"
              "_" "_" "_"]
      (create-board 3)))

  (it "has 16 cells for a 4x4 board"
    (should= ["_" "_" "_" "_"
              "_" "_" "_" "_"
              "_" "_" "_" "_"
              "_" "_" "_" "_"]
      (create-board 4)))

  (it "has the number of cells passed to the function"
    (should= ["_" "_" "_" "_" "_"
              "_" "_" "_" "_" "_"
              "_" "_" "_" "_" "_"
              "_" "_" "_" "_" "_"
              "_" "_" "_" "_" "_"]
      (create-board 5)))

  (it "creates a numbered board given a specific size"
    (should= ["0" "1" "2" "3" "4"
              "5" "6" "7" "8" "9"
              "10" "11" "12" "13" "14"
              "15" "16" "17" "18" "19"
              "20" "21" "22" "23" "24"]
      (numbered-board 5)))

  (it "sets a X mark at the correct position"
    (should= ["X" "_" "_"
              "_" "_" "_"
              "_" "_" "_"]
      (set-position (create-board 3) 0 "X")))

  (it "sets an X mark followed by an O mark in the correct positions"
    (should= ["X" "O" "_"
              "_" "_" "_"
              "_" "_" "_"]
      (set-position (set-position (create-board 3) 0 "X") 1 "O")))

  (it "sets an O mark in the correct position"
    (should= ["O" "_" "_"
              "_" "_" "_"
              "_" "_" "_"]
      (set-position (create-board 3) 0 "O")))


  (it "adds pipe signs between numbers for a 4x4 board"
    (should= [["0" " | " "1" " | " "2" " | " "3"]
              ["4" " | " "5" " | " "6" " | " "7"]
              ["8" " | " "9" " | " "10" " | " "11"]
              ["12" " | " "13" " | " "14" " | " "15"]]
    (insert-pipe-sign (numbered-board 4) 4)))

  (it "inserts dividers between rows for a 4x4 board"
    (should= [["0" " | " "1" " | " "2" " | " "3"]
              "\n-------------\n"
              ["4" " | " "5" " | " "6" " | " "7"]
              "\n-------------\n"
              ["8" " | " "9" " | " "10" " | " "11"]
              "\n-------------\n"
              ["12" " | " "13" " | " "14" " | " "15"]]
    (insert-dividers (numbered-board 4) 4)))

  (it "generates a divider of the same length as a piped row"
    (should= "\n-------------\n"
      (generate-divider (numbered-board 4) 4)))

  (it "is displayed on three lines with separators when empty"
    (should=
      "_ | _ | _\n---------\n_ | _ | _\n---------\n_ | _ | _"
      (format-board (create-board 3) 3)))

  (it "is displayed on three lines with separators when numbered"
    (should=
      "0 | 1 | 2\n---------\n3 | 4 | 5\n---------\n6 | 7 | 8"
      (format-board (numbered-board 3) 3)))

  (it "is displayed on four lines and four columns with separators when numbered"
    (should=
      "0 | 1 | 2 | 3\n-------------\n4 | 5 | 6 | 7\n-------------\n8 | 9 | 10 | 11\n-------------\n12 | 13 | 14 | 15"
      (format-board (numbered-board 4) 4)))

  (it "prompts the user to choose a position within the range of a 5x5 board"
    (should= "Please choose a position between 0 and 24: "
    (select-position ["0" "1" "2" "3" "4"
                      "5" "6" "7" "8" "9"
                      "10" "11" "12" "13" "14"
                      "15" "16" "17" "18" "19"
                      "20" "21" "22" "23" "24"])))

  (it "prompts the user to choose another position within the range of a 5x5 board"
    (should= "This number is invalid, please enter a number between 0 and 24: "
    (invalid-position-selection ["0" "1" "2" "3" "4"
                      "5" "6" "7" "8" "9"
                      "10" "11" "12" "13" "14"
                      "15" "16" "17" "18" "19"
                      "20" "21" "22" "23" "24"]))))

(describe "A UI"
  (it "prompts a player to pick a symbol"
    (should= "Y"
      (with-in-str "Y" (prompt-user "Fake prompt "))))

  (it "prompts a human player to pick a position on the board"
    (should= 1
      (with-in-str "1" (get-human-position ["O" "_" "_"
                                            "_" "X" "_"
                                            "_" "_" "_"]

                                            ["0" "1" "2"
                                             "3" "4" "5"
                                             "6" "7" "8"]

                                            "Fake prompt "))))

  (it "returns true when a position is empty"
    (should= true
      (position-empty? ["O" "_" "_"
                        "_" "_" "_"
                        "_" "_" "_"] 4)))

  (it "returns false when a position is not empty"
    (should= false
      (position-empty? ["O" "_" "_"
                        "_" "X" "_"
                        "_" "_" "_"] 4)))

  (it "returns the user's input if it is valid"
    (should= 3
      (with-in-str "3" (get-human-position ["O" "_" "_"
                                            "_" "X" "_"
                                            "_" "_" "_"]

                                            ["0" "1" "2"
                                             "3" "4" "5"
                                             "6" "7" "8"]

                                             "Fake prompt "))))

  (it "returns true if the input for position selection is valid"
    (should= true
      (valid-position-selection? "1" ["0" "1" "2"
                                      "3" "4" "5"
                                      "6" "7" "8"])))

  (it "returns false if the input for position selection is invalid because beyond range"
    (should= false
      (valid-position-selection? "9" ["0" "1" "2"
                                      "3" "4" "5"
                                      "6" "7" "8"])))

  (it "returns false if the input for position selection is invalid because too long"
    (should= false
      (valid-position-selection? "1234" ["0" "1" "2"
                                         "3" "4" "5"
                                         "6" "7" "8"])))

  (it "returns false if the input for position selection is invalid because not numerical"
    (should= false
      (valid-position-selection? "g" ["0" "1" "2"
                                      "3" "4" "5"
                                      "6" "7" "8"])))

  (it "returns true if the input for position selection is valid on 4x4 board on 4x4 board"
    (should= true
      (valid-position-selection? "1" ["0" "1" "2" "3"
                                          "4" "5" "6" "7"
                                          "8" "9" "10" "11"
                                          "12" "13" "14" "15"])))

  (it "returns false if the input for position selection is invalid because beyond range on 4x4 board"
    (should= false
      (valid-position-selection? "16" ["0" "1" "2" "3"
                                           "4" "5" "6" "7"
                                           "8" "9" "10" "11"
                                           "12" "13" "14" "15"])))

  (it "returns false if the input for positive position selection is invalid because beyond range on 4x4 board"
    (should= false
      (valid-position-selection? "-1" ["0" "1" "2" "3"
                                           "4" "5" "6" "7"
                                           "8" "9" "10" "11"
                                           "12" "13" "14" "15"])))

  (it "returns false if the input for position selection is invalid because too long on 4x4 board"
    (should= false
      (valid-position-selection? "1234" ["0" "1" "2" "3"
                                             "4" "5" "6" "7"
                                             "8" "9" "10" "11"
                                             "12" "13" "14" "15"])))

  (it "returns false if the input for position selection is invalid because not numerical on 4x4 board"
    (should= false
      (valid-position-selection? "g" ["0" "1" "2" "3"
                                          "4" "5" "6" "7"
                                          "8" "9" "10" "11"
                                          "12" "13" "14" "15"])))

  (it "returns true if the input for player selection is valid with an uppercase letter"
    (should= true
      (valid-player-selection? "C")))

  (it "returns true if the input for player selection is valid with a lowercase letter"
    (should= true
      (valid-player-selection? "h")))

  (it "returns false if the input for player selection is invalid because it's not C or H"
    (should= false
      (valid-player-selection? "Z")))

  (it "returns false if the input for player selection is invalid because it's too long"
    (should= false
      (valid-player-selection? "hello")))

  (it "returns false if the input for player selection is invalid because it's numerical"
    (should= false
      (valid-player-selection? "123")))

  (it "returns an integer if the input is valid"
    (should= 5
      (with-in-str "5" (pick-board-size "Fake prompt")))))

(describe "A decision engine"
  (it "gets the rows from a 3x3 board"
    (should= [["X" "_" "_"] ["O" "_" "_"] ["X" "_" "_"]]
      (get-rows ["X" "_" "_"
                 "O" "_" "_"
                 "X" "_" "_"] 3)))

 (it "gets the rows from a 4x4 board"
   (should= [["X" "_" "_" "_"] ["O" "_" "_" "_"] ["X" "_" "_" "_"] ["O" "_" "_" "_"]]
     (get-rows ["X" "_" "_" "_"
                "O" "_" "_" "_"
                "X" "_" "_" "_"
                "O" "_" "_" "_"] 4)))

  (it "gets the columns from a board"
    (should= [["X" "_" "_"] ["O" "_" "_"] ["X" "_" "_"]]
      (get-columns ["X" "O" "X"
                    "_" "_" "_"
                    "_" "_" "_"] 3)))

  (it "gets the columns from a 4x4 board"
    (should= [["X" "_" "_" "_"] ["O" "_" "_" "_"] ["X" "_" "_" "_"] ["O" "_" "_" "_"]]
      (get-columns ["X" "O" "X" "O"
                    "_" "_" "_" "_"
                    "_" "_" "_" "_"
                    "_" "_" "_" "_"] 4)))

  (it "should return a vector with the indices of the cells in a left to right diagonal for a 4x4 board"
    (should= [0 5 10 15]
      (get-left-diagonal 4)))

  (it "should return a vector with the indices of the cells in a left to right diagonal for a 5x5 board"
    (should= [0 6 12 18 24]
      (get-left-diagonal 5)))

  (it "should return a vector with the indices of the cells in a right to left diagonal for a 4x4 board"
    (should= [3 6 9 12]
      (get-right-diagonal 4)))

  (it "should return a vector with the indices of the cells in a right to left diagonal for a 5x5 board"
    (should= [4 8 12 16 20]
      (get-right-diagonal 5)))

  (it "gets the diagonals from a 3x3 board"
      (should= [["X" "O" "X"] ["O" "O" "O"]]
        (get-diagonals ["X" "_" "O"
                        "_" "O" "_"
                        "O" "_" "X"] 3)))

  (it "gets the diagonals from a 4x4 board"
      (should= [["X" "X" "X" "X"] ["O" "O" "O" "O"]]
        (get-diagonals ["X" "_" "_" "O"
                        "_" "X" "O" "_"
                        "_" "O" "X" "_"
                        "O" "_" "_" "X"] 4)))

  (it "joins the rows, columns and diagonals"
    (should=
      [["X" "_" "O"] ["_" "X" "_"] ["O" "_" "X"]
       ["X" "_" "O"] ["_" "X" "_"] ["O" "_" "X"]
       ["X" "X" "X"] ["O" "X" "O"]]
      (join-sections ["X" "_" "O"
                      "_" "X" "_"
                      "O" "_" "X"] 3)))

  (it "joins the rows, columns and diagonals for a 4x4 board"
    (should=
      [["X" "_" "_" "O"] ["_" "X" "O" "_"] ["_" "O" "X" "_"] ["O" "_" "_" "X"]
       ["X" "_" "_" "O"] ["_" "X" "O" "_"] ["_" "O" "X" "_"] ["O" "_" "_" "X"]
       ["X" "X" "X" "X"] ["O" "O" "O" "O"]]
      (join-sections ["X" "_" "_" "O"
                      "_" "X" "O" "_"
                      "_" "O" "X" "_"
                      "O" "_" "_" "X"] 4)))

  (it "returns true if the symbols are equal for a 3x3 board"
    (should= true
      (symbols-equal? ["X" "X" "X"] "X")))

  (it "returns false if the symbols are not equal for a 3x3 board"
    (should= false
      (symbols-equal? ["X" "_" "X"] "X")))

  (it "returns false if there are no symbols for a 3x3 board"
    (should= false
      (symbols-equal? ["_" "_" "_"] "X")))

  (it "returns true if the symbols are equal for a 4x4 board"
    (should= true
      (symbols-equal? ["X" "X" "X" "X"] "X")))

  (it "returns false if the symbols are not equal for a 4x4 board"
    (should= false
      (symbols-equal? ["X" "_" "X" "_"] "X")))

  (it "returns false if there are no symbols for a 4x4 board"
    (should= false
      (symbols-equal? ["_" "_" "_" "_"] "X")))

  (it "checks if three symbols are aligned"
    (should= true
      (n-aligned? ["X" "_" "O"
                       "_" "X" "_"
                       "O" "_" "X"] "X" 3)))

  (it "checks if three symbols are aligned"
    (should= true
      (n-aligned? ["X" "_" "_" "_"
                       "_" "X" "_" "_"
                       "_" "_" "X" "_"
                       "_" "_" "_" "X"] "X" 4)))

  (it "returns true if a 3x3 board is full"
    (should= true
      (board-full? ["X" "O" "X"
                    "O" "X" "O"
                    "X" "O" "X"])))

  (it "returns true if a 4x4 board is full"
    (should= true
      (board-full? ["X" "O" "X" "X"
                    "O" "X" "O" "O"
                    "X" "O" "X" "X"
                    "X" "O" "X" "X"])))

  (it "returns false if a 3x3 board is not full"
    (should= false
      (board-full? ["X" "O" "X"
                    "O" "X" "O"
                    "X" "O" "_"])))

  (it "returns false if a 4x4 board is not full"
    (should= false
      (board-full? ["X" "O" "X" "X"
                    "O" "X" "_" "O"
                    "X" "O" "X" "X"
                    "X" "O" "X" "X"])))

  (it "returns true if the game is over because three symbols are aligned"
    (should= true
      (game-over? ["X" "_" "O"
                   "_" "X" "_"
                   "O" "_" "X"] "X" "O" 3)))

  (it "returns true if the game is over because the board is full"
    (should= true
      (game-over? ["X" "O" "X"
                   "O" "X" "O"
                   "X" "O" "X"] "X" "O" 3)))

  (it "returns false if the game is not over and no three symbols are aligned"
   (should= false
     (game-over? ["X" "_" "O"
                  "_" "_" "_"
                  "O" "_" "X"] "X" "O" 3)))

 (it "returns true if the game is over because four symbols are aligned"
   (should= true
     (game-over? ["X" "O" "X" "X"
                  "O" "X" "_" "O"
                  "X" "O" "X" "X"
                  "X" "O" "X" "X"] "X" "O" 4)))

 (it "returns true if the game is over because the 4x4 board is full"
   (should= true
     (game-over? ["X" "O" "X" "X"
                  "O" "X" "X" "O"
                  "X" "O" "O" "X"
                  "X" "O" "X" "X"] "X" "O" 4)))

  (it "returns false if the game is not over and no four symbols are aligned"
   (should= false
     (game-over? ["X" "O" "X" "X"
                  "O" "_" "_" "_"
                  "X" "O" "_" "X"
                  "O" "_" "X" "_"] "X" "O" 4))))

(describe "A game"
  (it "tests a Human VS Human game"
      (let [output (with-out-str (with-in-str "h\nh\n0\n2\n3\n5\n6" (start-game)))
            board-state-1 (create-board)
            board-state-2 (set-position board-state-1 0 "X")
            board-state-3 (set-position board-state-2 2 "O")
            board-state-4 (set-position board-state-3 3 "X")
            board-state-5 (set-position board-state-4 5 "O")
            board-state-6 (set-position board-state-5 6 "X")]
        (should=
          (str select-first-player
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

  (it "tests a Human VS Human game where the position and player input are incorrect at first"
      (let [output (with-out-str (with-in-str "h\nz\n4\nh\n0\n0\nm\n2\n3\n5\n6" (start-game)))
            board-state-1 (create-board)
            board-state-2 (set-position board-state-1 0 "X")
            board-state-3 (set-position board-state-2 2 "O")
            board-state-4 (set-position board-state-3 3 "X")
            board-state-5 (set-position board-state-4 5 "O")
            board-state-6 (set-position board-state-5 6 "X")]
        (should=
          (str select-first-player
               select-opponent
               invalid-player-selection
               invalid-player-selection
               (format-board numbered-board)
               (format-board board-state-1)
               select-position
               (format-board board-state-2)
               select-position
               occupied-position
               invalid-position-selection
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
      (let [output (with-out-str (with-in-str "h\nc\n0\n1\n6\n5\n7" (start-game)))
            board-state-1 (create-board)
            board-state-2 (set-position board-state-1 0 "X")
            board-state-3 (set-position board-state-2 (get-computer-position board-state-2 "O" "X") "O")
            board-state-4 (set-position board-state-3 1 "X")
            board-state-5 (set-position board-state-4 (get-computer-position board-state-4 "O" "X") "O")
            board-state-6 (set-position board-state-5 6 "X")
            board-state-7 (set-position board-state-6 (get-computer-position board-state-6 "O" "X") "O")
            board-state-8 (set-position board-state-7 5 "X")
            board-state-9 (set-position board-state-8 (get-computer-position board-state-8 "O" "X") "O")
            board-state-10 (set-position board-state-9 7 "X")]
        (should=
          (str select-first-player
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
               (format-board board-state-7)
               select-position
               (format-board board-state-8)
               (format-board board-state-9)
               select-position
               (format-board board-state-10)
               end-game"\n")
          output)))

  (it "tests a Computer VS Human game"
      (let [output (with-out-str (with-in-str "c\nh\n4\n6\n5\n1" (start-game)))
            board-state-1 (create-board)
            board-state-2 (set-position board-state-1 (get-computer-position board-state-1 "X" "O") "X")
            board-state-3 (set-position board-state-2 4 "O")
            board-state-4 (set-position board-state-3 (get-computer-position board-state-3 "X" "O") "X")
            board-state-5 (set-position board-state-4 6 "O")
            board-state-6 (set-position board-state-5 (get-computer-position board-state-5 "X" "O") "X")
            board-state-7 (set-position board-state-6 5 "O")
            board-state-8 (set-position board-state-7 (get-computer-position board-state-7 "X" "O") "X")
            board-state-9 (set-position board-state-8 1 "O")
            board-state-10 (set-position board-state-9 (get-computer-position board-state-9 "X" "O") "X")]
        (should=
          (str select-first-player
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
               (format-board board-state-8)
               select-position
               (format-board board-state-9)
               (format-board board-state-10)
               end-game"\n")
          output))))

(describe "Minimax"
  (it "returns a winning position for player X on the board"
    (should= 8
      (get-computer-position ["O" "_" "X"
                              "O" "_" "X"
                              "X" "_" "_"]

                             ["0" "1" "2"
                              "3" "4" "5"
                              "6" "7" "8"]

                             "X" "O" 0 3)))

  (it "returns a winning position for player X when there are three spots left"
    (should= 6
      (get-computer-position ["O" "X" "_"
                              "O" "O" "_"
                              "_" "X" "X"]

                              ["0" "1" "2"
                               "3" "4" "5"
                               "6" "7" "8"]

                               "X" "O" 0 3)))

  (it "returns a position and positive score when a winning position is available"
    (should= 8
      (get-computer-position ["O" "X" "X"
                              "O" "O" "X"
                              "X" "O" "_"]


                             ["0" "1" "2"
                              "3" "4" "5"
                              "6" "7" "8"]

                             "X" "O" 0 3)))

  (it "returns a position and negative score when the opponent is in a winning position"
    (should= 8
      (get-computer-position ["O" "X" "O"
                              "O" "X" "X"
                              "O" "O" "_"]


                             ["0" "1" "2"
                              "3" "4" "5"
                              "6" "7" "8"]

                             "X" "O" 0 3)))

  (it "returns a position and 0 when the game is a draw"
    (should= 8
      (get-computer-position ["O" "X" "O"
                              "O" "X" "X"
                              "X" "O" "_"]


                             ["0" "1" "2"
                              "3" "4" "5"
                              "6" "7" "8"]

                             "X" "O" 0 3)))

  (it "returns a score of 10 if the game is a win"
    (should= 10
      (evaluate-board ["X" "_" "_"
                       "X" "O" "O"
                       "X" "_" "_"] "X" "O" 3 3)))

  (it "returns a score of -10 if the game is a loss"
    (should= -10
      (evaluate-board ["X" "_" "_"
                       "O" "O" "O"
                       "X" "_" "_"] "X" "O" 3 3)))

  (it "returns a score of zero if the game is a draw"
    (should= 0
      (evaluate-board ["X" "X" "O"
                       "O" "O" "X"
                       "X" "O" "_"] "X" "O" 3 3)))

  (it "returns a score of 10 if the game is a win"
   (should= 10
     (evaluate-board ["X" "X" "X" "X"
                      "O" "X" "_" "O"
                      "X" "O" "X" "X"
                      "X" "O" "X" "X"] "X" "O" 0 4)))

  (it "returns a score of -10 if the game is a loss"
   (should= -10
     (evaluate-board ["X" "O" "X" "X"
                      "O" "O" "_" "O"
                      "X" "O" "X" "X"
                      "X" "O" "X" "X"] "X" "O" 0 4)))

  (it "returns a score of zero if the game is a draw"
   (should= 0
     (evaluate-board ["X" "O" "X" "X"
                      "O" "X" "_" "O"
                      "X" "O" "O" "X"
                      "X" "O" "X" "X"] "X" "O" 0 4)))
;
  (it "returns a hash that includes the position and the score of the move when there is a winning move"
    (should= {5 10}
      (score-move ["O" "X" "X"
                   "O" "_" "_"
                   "_" "O" "X"] 5 "X" "O" 0 3)))

  (it "returns a hash that includes the position and the score of the move when there is a neutral move"
    (should= {4 0}
      (score-move ["X" "O" "X"
                   "O" "_" "_"
                   "_" "X" "O"] 4 "X" "O" 0 3)))

  (it "returns a hash that includes the position and the score of the move when there is a losing move"
    (should= {4 -10}
      (score-move ["O" "X" "X"
                   "O" "_" "_"
                   "O" "O" "X"] 4 "X" "O" 0 3)))

  (it "returns a list of scored positions in hashes"
    (should= {3 0, 4 -10, 5 10}
      (scored-moves ["O" "X" "X"
                     "_" "_" "_"
                     "O" "O" "X"] "X" "O" 0 3)))

  (it "returns an empty list if the board is full"
    (should= {}
      (scored-moves ["O" "X" "X"
                     "X" "O" "O"
                     "O" "O" "X"] "X" "O" 0 3)))
;
  (it "returns a winning position for the maximising player"
    (should= 5
      (choose-best-move ["O" "X" "X"
                         "_" "_" "_"
                         "O" "O" "X"] "X" "O" 0 3)))

  (it "returns a position that will make the maximising player lose"
    (should= 3
      (choose-best-move ["O" "X" "X"
                         "_" "_" "_"
                         "O" "O" "X"] "O" "X" 0 3)))

  (it "returns a position that will make the maximising player lose"
    (should= 5
      (choose-best-move ["O" "X" "X"
                         "X" "_" "_"
                         "_" "O" "X"] "O" "X" 0 3)))

  (it "does not loose"
     (doseq [board (all-computer-boards (create-board 3) "X" "0")]
       (should (>= 0 (evaluate-board board "O" "X" 0 3)))))

  (it "returns a score of 0 when it reaches the max depth"
    (should= 0
      (minimax ["_" "X" "_" "O"
                "_" "X" "_" "_"
                "_" "_" "O" "_"
                "X" "_" "X" "_"] "X" "O" 0 4))))
