(ns tic-tac-toe-clojure.core
  (:require [clojure.string :as string]))

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

(defn find-empty-spots [board]
  (filter (fn [[_ marker]] (= "_" marker)) (map-indexed vector board)))

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
  (concat (get-rows board) (concat (get-columns board)) (concat (get-diagonals board))))

(defn symbols-equal? [section symbol]
  (and (= (nth section 0) (nth section 1) (nth section 2) symbol) (not= (apply str section) "___")))

(defn three-aligned? [board symbol]
  (some true? (for [section (join-sections board)] (symbols-equal? section symbol))))

(defn board-full? [board]
  (if (some #{"_"} board) false true))

(def end-game "Game is over")

(defn game-over? [board symbol]
  (or (three-aligned? board symbol) (board-full? board)))


(defn evaluate-board [board current-player opponent]
  (cond
    (three-aligned? board current-player) 10
    (three-aligned? board opponent) -10
    :else 0))


(defn score-move [board position current-player opponent]
  {position (minimax (set-position board position current-player) current-player opponent)})



(defn get-computer-position [board current-player opponent]

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
