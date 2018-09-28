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

(def invalid-4x4-position-selection "This number is invalid, please enter a number between 0 and 8: ")

(defn valid-position-selection? [input]
  (boolean (some #{input} ["0" "1" "2" "3" "4" "5" "6" "7" "8"])))

  (defn valid-4x4-position-selection? [input]
    (boolean (some #{input} ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15"])))

(defn position-empty? [board position]
  (= (nth board position) "_"))

(defn prompt-user [prompt]
  (do (print prompt) (flush) (read-line)))

(defn get-human-position [board prompt]
  (let [user-input (prompt-user prompt)]
      (if (valid-position-selection? user-input)
          (if (position-empty? board (Integer/parseInt user-input))
                (Integer/parseInt user-input)
                (get-human-position board occupied-position))
          (get-human-position board invalid-position-selection))))

(defn find-empty-spots [board]
  (filter (fn [[_ marker]] (= "_" marker)) (map-indexed vector board)))

(def set-position assoc)

(def select-first-player "Please select the first player to take a turn (H/h for human or C/c for computer): ")

(def select-opponent "Please select an opponent: ")

(def invalid-player-selection "Invalid choice, please choose H/h for human or C/c for computer: ")

(defn valid-player-selection? [input]
  (boolean (some #{input} ["C" "c" "h" "H"])))

(defn get-rows [board]
  (partition 3 board))

(defn get-4x4-rows [board]
  (partition 4 board))

(defn get-columns [board]
  (apply map vector (get-rows board)))

(defn get-4x4-columns [board]
  (apply map vector (get-4x4-rows board)))

(defn- symbols-at [board positions]
  (map (fn [position] (nth board position)) positions))

(defn get-diagonals [board]
  [(symbols-at board [0 4 8])
   (symbols-at board [2 4 6])])

(defn get-4x4-diagonals [board]
 [(symbols-at board [0 5 10 15])
  (symbols-at board [3 6 9 12])])

(defn join-sections [board]
  (concat (get-rows board) (concat (get-columns board)) (concat (get-diagonals board))))

(defn join-4x4-sections [board]
  (concat (get-4x4-rows board) (concat (get-4x4-columns board)) (concat (get-4x4-diagonals board))))

(defn symbols-equal? [section symbol]
  (and (= (nth section 0) (nth section 1) (nth section 2) symbol) (not= (apply str section) "___")))

(defn symbols-4x4-equal? [section symbol]
  (and (= (nth section 0) (nth section 1) (nth section 2) (nth section 3) symbol) (not= (apply str section) "____")))

(defn three-aligned? [board symbol]
  (some true? (for [section (join-sections board)] (symbols-equal? section symbol))))

(defn four-aligned? [board symbol]
  (some true? (for [section (join-4x4-sections board)] (symbols-4x4-equal? section symbol))))

(defn board-full? [board]
  (if (some #{"_"} board) false true))

(def end-game "Game is over")

(defn game-over? [board symbol1 symbol2]
  (or (three-aligned? board symbol1) (three-aligned? board symbol2) (board-full? board)))

(defn game-over-4x4? [board symbol1 symbol2]
  (or (four-aligned? board symbol1) (four-aligned? board symbol2) (board-full? board)))

(defn get-empty-spots [board]
  (remove #{"_"} (flatten (find-empty-spots board))))

(defn evaluate-board [board current-player opponent]
  (cond
    (three-aligned? board current-player) 10
    (three-aligned? board opponent) -10
    :else 0))

(defn evaluate-4x4-board [board current-player opponent]
  (cond
    (four-aligned? board current-player) 10
    (four-aligned? board opponent) -10
    :else 0))

(defn minimax [board maximising-player minimising-player]
    (if (game-over-4x4? board maximising-player minimising-player)
      (evaluate-4x4-board board maximising-player minimising-player)
      (* -1 (val (apply max-key val (scored-moves board minimising-player maximising-player))))))

(defn score-move [board position current-player opponent]
  {position (minimax (set-position board position current-player) current-player opponent)})

(defn scored-moves [board current-player opponent]
  (let [empty-spots (get-empty-spots board)]
    (into (sorted-map) (map #(score-move board % current-player opponent) empty-spots))))

(defn choose-best-move [board current-player opponent]
  (key (apply max-key val (scored-moves board current-player opponent))))

(defn get-computer-position [board current-player opponent]
  (choose-best-move board current-player opponent))

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
    (get-computer-position board symbol (get-symbol opponent))))

(defn make-computer-player [symbol] (ComputerPlayer. symbol))

(defn next-player-turn [board current-player opponent]
  (display-board board)
  (if (game-over-4x4? board (get-symbol opponent) (get-symbol current-player))
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
    (next-player-turn (create-board) player-1 player-2)))
