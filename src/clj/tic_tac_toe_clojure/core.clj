(ns tic-tac-toe-clojure.core
  (:require [clojure.string :as string]))

(declare start-game)
(declare scored-moves)
(declare score-move)

(defn -main [& args]
  (println "Starting game")
  (start-game))

(defn create-board []
  (into [] (repeat 9 "_")))

(defn create-4x4-board []
  (into [] (repeat 16 "_")))

(defn create-sized-board [size]
  (into [] (repeat (* size size) "_")))

(def numbered-board
  ["0" "1" "2" "3" "4" "5" "6" "7" "8"])

(def numbered-4x4-board
  ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15"])

(defn numbered-sized-board [board]
  (let [size (count board)]
    (for [cell (take size (range))] (str cell))))

(defn format-board [board]
  (apply str (flatten (for [part (partition 3 board)] (flatten (map vector part '(" | " " | " "\n---------\n")))))))

(defn format-4x4-board [board]
  (apply str (flatten (for [part (partition 4 board)] (flatten (map vector part '(" | " " | " " | " "\n--------------\n")))))))

(defn display-board [board]
  (print (format-4x4-board board)))

(def select-position "Please choose a position between 0 and 8: ")

(def select-4x4-position "Please choose a position between 0 and 15: ")

(def occupied-position "This position is occupied, please select another: ")

(def invalid-position-selection "This number is invalid, please enter a number between 0 and 8 ")

(def invalid-4x4-position-selection "This number is invalid, please enter a number between 0 and 15: ")

(def select-board-size "Please enter a number greater than 0 to determine the size of your board: ")

(def invalid-board-size "This number is invalid, pleaser enter a number greater than 0: ")

(defn valid-position-selection? [input]
  (boolean (some #{input} ["0" "1" "2" "3" "4" "5" "6" "7" "8"])))

(defn valid-4x4-position-selection? [input]
  (boolean (some #{input} ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15"])))

(defn position-empty? [board position]
  (= (nth board position) "_"))

(defn prompt-user [prompt]
  (do (print prompt) (flush) (read-line)))

(defn pick-board-size [prompt]
  (let [user-input (prompt-user prompt)]
    (if (boolean (re-seq #"^[1-9]+[0-9]*$" user-input))
        (Integer/parseInt user-input)
        (pick-board-size invalid-board-size))))

(defn get-human-position [board prompt]
  (let [user-input (prompt-user prompt)]
      (if (valid-4x4-position-selection? user-input)
          (if (position-empty? board (Integer/parseInt user-input))
                (Integer/parseInt user-input)
                (get-human-position board occupied-position))
          (get-human-position board invalid-4x4-position-selection))))

(defn find-empty-spots [board]
  (filter (fn [[_ marker]] (= "_" marker)) (map-indexed vector board)))

(def set-position assoc)

(def select-first-player "Please select the first player to take a turn (H/h for human or C/c for computer): ")

(def select-opponent "Please select an opponent: ")

(def invalid-player-selection "Invalid choice, please choose H/h for human or C/c for computer: ")

(defn valid-player-selection? [input]
  (boolean (some #{input} ["C" "c" "h" "H"])))

(defn get-rows [board size]
  (partition size board))

(defn get-left-diagonal [size]
  (reduce (fn [a b] (conj a (+' (last a) (+ 1 size)))) [0 (+ 1 size)] (range (- size 2))))

(defn get-right-diagonal [size]
  (next (drop-last (reduce (fn [a b] (conj a (+' (last a) (- size 1)))) [(- size 1) (- size 1)] (range size)))))

(defn get-nxn-columns [board size])

(defn get-columns [board size]
  (apply map vector (get-rows board size)))

(defn- symbols-at [board positions]
  (map (fn [position] (nth board position)) positions))

(defn get-diagonals [board size]
  [(symbols-at board (get-left-diagonal size))
   (symbols-at board (get-right-diagonal size))])

(defn join-sections [board size]
  (concat (get-rows board size) (concat (get-columns board size)) (concat (get-diagonals board size))))

(defn symbols-equal? [section symbol]
  (every? #{symbol} section))

(defn n-aligned? [board symbol size]
  (some true? (for [section (join-sections board size)] (symbols-equal? section symbol))))

(defn board-full? [board]
  (if (some #{"_"} board) false true))

(def end-game "Game is over")

(defn game-over? [board symbol1 symbol2 size]
  (or (n-aligned? board symbol1 size) (n-aligned? board symbol2 size) (board-full? board)))

(defn get-empty-spots [board]
  (remove #{"_"} (flatten (find-empty-spots board))))

(def start-depth 0)

(def max-depth 4)

(defn evaluate-board [board current-player opponent depth size]
  (cond
    (= depth max-depth) 0
    (n-aligned? board current-player size) 10
    (n-aligned? board opponent size) -10
    :else 0))

(defn minimax [board maximising-player minimising-player depth size]
    (if (or (= depth max-depth) (game-over? board maximising-player minimising-player size))
      (evaluate-board board maximising-player minimising-player depth size)
          (* -1 (val (apply max-key val (scored-moves board minimising-player maximising-player (inc depth) size))))))

(defn score-move [board position current-player opponent depth size]
  {position (minimax (set-position board position current-player) current-player opponent depth size)})

(defn scored-moves [board current-player opponent depth size]
  (let [empty-spots (get-empty-spots board)]
    (into (sorted-map) (map #(score-move board % current-player opponent depth size) empty-spots))))

(defn choose-best-move [board current-player opponent depth size]
  (key (apply max-key val (scored-moves board current-player opponent depth size))))

(defn get-computer-position [board current-player opponent depth size]
  (choose-best-move board current-player opponent depth size))

(defprotocol Player
  (get-symbol [this])
  (get-move [this board opponent]))

(deftype HumanPlayer [symbol]
  Player
  (get-symbol [this] symbol)
  (get-move [this board opponent]
    (get-human-position board select-4x4-position)))

(defn make-human-player [symbol] (HumanPlayer. symbol))

(deftype ComputerPlayer [symbol]
  Player
  (get-symbol [this] symbol)
  (get-move [this board opponent]
    (get-computer-position board symbol (get-symbol opponent) start-depth)))

(defn make-computer-player [symbol] (ComputerPlayer. symbol))

(defn next-player-turn [board current-player opponent]
  (display-board board)
  (if (game-over? board (get-symbol opponent) (get-symbol current-player))
    (println end-game)
    (recur
      (set-position board
                    (get-move current-player board opponent)
                    (get-symbol current-player))
      opponent
      current-player)))

(defn get-player [marker prompt]
  (let [player-type (prompt-user prompt)]
  (if (valid-player-selection? player-type)
      (if (= player-type "h")
        (HumanPlayer. marker)
        (ComputerPlayer. marker))
        (get-player marker invalid-player-selection))))

(defn start-game []
  (let [player-1 (get-player "X" select-first-player)
        player-2 (get-player "O" select-opponent)]
    (display-board numbered-4x4-board)
    (next-player-turn (create-4x4-board) player-1 player-2)))
