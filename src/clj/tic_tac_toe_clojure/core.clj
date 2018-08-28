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
  (with-out-str (println (string/join "\n" (re-seq #".{1,3}" (apply str board))))))

(defn display-board [board]
  (print (format-board board)))

;(defn get-player-type []
; (do (print "Please select an opponent (H for human or C for computer): ") (flush) (read-line)))

(defn get-player-symbol []
  (do (print "Please choose a symbol (X or O): ") (flush) (read-line)))

(defn get-human-position []
 (Integer/parseInt (do (print "Please choose a position between 0 and 8: ") (flush) (read-line))))

(defn get-computer-position [board randomiser]
  (get (randomiser (filter (fn [[_ marker]] (= "_" marker)) (map-indexed vector board))) 0))

(defn get-player-position [board player-type]
  (if (= "h" player-type) (get-human-position) (get-computer-position board rand-nth)))

(def set-position assoc)

(defn get-opponent []
  (do (print "Please select an opponent (H for human or C for computer): ") (flush) (read-line)))

(defn get-game-type [opponent]
  (with-out-str(print opponent)))

(defn swap-type [player-type]
  (if (= "h" player-type) "c" "h"))

(defn swap-player-type [game-type player-type]
  (if (="h" game-type) "h" (swap-type player-type)))


(defn swap-player-symbol [player-symbol]
  (if (= "X" player-symbol) "O" "X"))

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

(defn symbols-equal? [section]
  (and (= (nth section 0) (nth section 1) (nth section 2)) (not= (apply str section) "___")))

(defn three-aligned? [board]
  (some true? (for [section (join-sections board)] (symbols-equal? section))))

(defn board-full? [board]
  (if (some #{"_"} board) false true))

(def end-game "Game is over")

(defn game-over? [board]
  (if (or (three-aligned? board) (board-full? board)) true false))

(defn next-player-turn [board player-symbol game-type player-type]
  (display-board board)
  (if (game-over? board)
    (println end-game)
    (next-player-turn (set-position board (get-player-position board player-type) player-symbol) (swap-player-symbol player-symbol) game-type (swap-player-type game-type player-type))))

(defn start-game []
  (let [opponent (get-opponent)]
    (let [game-type (get-game-type opponent)]
      (println game-type)
      (let [player-symbol (get-player-symbol)]
        (display-board numbered-board)
        (let [board (create-board)]
          (next-player-turn board player-symbol game-type opponent))))))

