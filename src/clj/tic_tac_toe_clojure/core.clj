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

(def end-game "Game is over")

(defn game-over? [board symbol]
  (if (or (three-aligned? board symbol) (board-full? board)) true false))

(defn score-move [board current-player opponent]
  (cond
    (three-aligned? board current-player) 10
    (three-aligned? board opponent) -10
    :else 0))

(defn calculate-score [score depth]
  (if (= 0 depth) score (/ score depth)))

(def maximising-value -10)

(def minimising-value 10)

(defn return-evaluated-score [evaluator value board current-player opponent depth]
  (evaluator value (calculate-score (score-move board current-player opponent) depth)))

(def depth 0)

(defn minimax [board current-player opponent depth evaluator value]
  (let [empty-spots (find-empty-spots board)]
    (if (> (count empty-spots) 1)
     (let [positions (for [spot empty-spots]
       (minimax (set-position board (first spot) current-player) opponent current-player (+ 1 depth) min minimising-value))]
     (first (filter (fn [tuple] (last tuple)) positions)))
     (let [position-index (first (first empty-spots))]
       [position-index (return-evaluated-score evaluator
                                               value
                                               (set-position board position-index current-player)
                                               current-player
                                               opponent
                                               depth)]))))

(defn get-computer-position [board current-player opponent]
  (first (minimax board current-player opponent depth max maximising-value)))

(defprotocol Player
  (get-symbol [this])
  (get-move [this board opponent]))

(deftype HumanPlayer [symbol]
  Player
  (get-symbol [this] symbol)
  (get-move [this board opponent]
    (get-human-position)))

(deftype ComputerPlayer [symbol]
  Player
  (get-symbol [this] symbol)
  (get-move [this board opponent]
    (get-computer-position board this opponent)))

(defn next-player-turn [board current-player opponent]
  (display-board board)
  (if (game-over? board (get-symbol opponent))
    (println end-game)
    (recur
      (set-position board
                    (get-move current-player board opponent)
                    (get-symbol current-player))
      opponent
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
