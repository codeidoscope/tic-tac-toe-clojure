(ns tic-tac-toe-clojure.core (:require [clojure.string :as string]))
(declare start-game)
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

(defn prompt-user [prompt]
  (do (print prompt) (flush) (read-line)))

(defn get-human-position []
  (Integer/parseInt (prompt-user select-position)))

; Not in use currently - might be needed in the future
;(defn get-computer-position [board randomiser]
;  (get (randomiser (filter (fn [[_ marker]] (= "_" marker)) (map-indexed vector board))) 0))

(defn find-empty-spots [board]
  (filter (fn [[_ marker]] (= "_" marker)) (map-indexed vector board)))

(defn get-first-available-position [board]
  (get (first (find-empty-spots board)) 0))

(def set-position assoc)

(def select-opponent "Please select an opponent (H for human or C for computer): ")

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
  (concat (get-rows board) (concat (get-columns board)) (concat(get-diagonals board))))

(defn symbols-equal? [section symbol]
  (and (= (nth section 0) (nth section 1) (nth section 2) symbol) (not= (apply str section) "___")))

(defn three-aligned? [board symbol]
  (some true? (for [section (join-sections board)] (symbols-equal? section symbol))))

(defn board-full? [board]
  (if (some #{"_"} board) false true))

(defn is-draw? [board position current-player opponent]
  (if-not (three-aligned? (set-position board position opponent) opponent) true false))

(def end-game "Game is over")

(defn game-over? [board symbol]
  (if (or (three-aligned? board symbol) (board-full? board)) true false))

(defn calculate-score [score depth]
  (/ score depth))

(defn score-move [board current-player opponent & {:keys [position]
                                                   :or {position nil}}]
  (cond
    (is-draw? board position current-player opponent) 0
    (three-aligned? board current-player) 10
    (three-aligned? board opponent) -10))


(defn return-position-scores [board current-player opponent]
  (let [empty-spots (find-empty-spots board)]
    (if (> (count empty-spots) 1)
     (let [positions (for [spot empty-spots]
       (return-position-scores (set-position board (first spot) current-player) opponent current-player))]
     (first (filter (fn [tuple] (last tuple)) positions)))
     (let [position-index (first (first empty-spots))]
       [position-index  (score-move (set-position board position-index current-player) current-player opponent :position position-index)]))))

(defn return-free-cells [board current-player opponent]
  (let [empty-spots (find-empty-spots board)]
    (if (> (count empty-spots) 1)
     (let [positions (for [spot empty-spots]
       (return-free-cells (set-position board (first spot) current-player) opponent current-player))]
       ; The following line returns the last element of the list of positions
     (first (filter (fn [tuple] (first tuple)) positions)))
     (let [position-index (first (first empty-spots))]
       [position-index (three-aligned? (set-position board position-index current-player) current-player)]))))

(defn return-position [board current-player opponent]
  (first (return-free-cells board current-player opponent)))

(defn return-score-position [board current-player opponent]
  (return-position-scores board current-player opponent))

(defprotocol Player
  (get-symbol [this])
  (get-move [this board]))

(deftype HumanPlayer [symbol]
  Player
  (get-symbol [this] symbol)
  (get-move [this board]
    (get-human-position)))

(deftype ComputerPlayer [symbol]
  Player
  (get-symbol [this] symbol)
  (get-move [this board]
    (get-first-available-position board)))

(defn next-player-turn [board current-player other-player]
  (display-board board)
  (if (game-over? board (get-symbol other-player))
    (println end-game)
    (recur
      (set-position board
                    (get-move current-player board)
                    (get-symbol current-player))
      other-player
      current-player)))

(defn get-player [marker]
  (let [player-type (prompt-user select-opponent)]
  (if (= player-type "h")
    (HumanPlayer. marker)
    (ComputerPlayer. marker))))

(defn start-game []
  (let [player-1 (get-player "X")
        player-2 (get-player "O")]
    (display-board numbered-board)
    (next-player-turn (create-board) player-2 player-1)))
