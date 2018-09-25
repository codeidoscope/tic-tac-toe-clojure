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

(def numbered-board
  ["0" "1" "2" "3" "4" "5" "6" "7" "8"])

(defn format-board [board]
  (apply str (flatten (for [part (partition 3 board)] (flatten (map vector part '(" | " " | " "\n---------\n")))))))

(defn display-board [board]
  (print (format-board board)))

(def select-position "Please choose a position between 0 and 8: ")

(def occupied-position "This position is occupied, please select another: ")

(def wrong-position-input "This number is invalid, please enter a number between 0 and 8 ")

(defn too-long? [input]
  (> (count input) 1))

(defn not-in-range? [input]
  (or (< input 0) (> input 8)))

(defn is-numeric? [input]
  (if-let [input (seq input)]
    (let [input (if (= (first input) \-) (next input) input)
          input (drop-while #(Character/isDigit %) input)
          input (if (= (first input) \.) (next input) input)
          input (drop-while #(Character/isDigit %) input)]
      (empty? input))))

(defn incorrect-length-and-range? [input]
  (or (too-long? input)
      (not-in-range? (Integer/parseInt input))))

(defn invalid-position-input? [input]
  (if (is-numeric? input)
      (incorrect-length-and-range? input) true))

(defn position-empty? [board position]
  (= (nth board position) "_"))

(defn prompt-user [prompt]
  (do (print prompt) (flush) (read-line)))

(defn get-human-position [board prompt]
  (let [user-input (prompt-user prompt)]
      (if (invalid-position-input? user-input)
          (get-human-position board wrong-position-input)
          (if (position-empty? board (Integer/parseInt user-input))
                (Integer/parseInt user-input)
                (get-human-position board occupied-position)))))

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

(defn get-columns [board]
  (apply map vector (get-rows board)))

(defn- symbols-at [board positions]
  (map (fn [position] (nth board position)) positions))

(defn get-diagonals [board]
  [(symbols-at board [0 4 8])
   (symbols-at board [2 4 6])])

(defn join-sections [board]
  (concat (get-rows board) (concat (get-columns board)) (concat (get-diagonals board))))

(defn symbols-equal? [section symbol]
  (and (= (nth section 0) (nth section 1) (nth section 2) symbol) (not= (apply str section) "___")))

(defn three-aligned? [board symbol]
  (some true? (for [section (join-sections board)] (symbols-equal? section symbol))))

(defn board-full? [board]
  (if (some #{"_"} board) false true))

(def end-game "Game is over")

(defn game-over? [board symbol1 symbol2]
  (or (three-aligned? board symbol1) (three-aligned? board symbol2) (board-full? board)))

(defn get-empty-spots [board]
  (remove #{"_"} (flatten (find-empty-spots board))))

(defn evaluate-board [board current-player opponent]
  (cond
    (three-aligned? board current-player) 10
    (three-aligned? board opponent) -10
    :else 0))

(defn minimax [board maximising-player minimising-player]
    (if (game-over? board maximising-player minimising-player)
      (evaluate-board board maximising-player minimising-player)
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
    (get-human-position board select-position)))

(defn make-human-player [symbol] (HumanPlayer. symbol))

(deftype ComputerPlayer [symbol]
  Player
  (get-symbol [this] symbol)
  (get-move [this board opponent]
    (get-computer-position board symbol (get-symbol opponent))))

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
    (display-board numbered-board)
    (next-player-turn (create-board) player-1 player-2)))
