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

(defn get-user-symbol []
  (do (print "Please choose a symbol (X or O): ") (flush) (read-line)))

(defn get-user-position []
 (Integer/parseInt (do (print "Please choose a position between 0 and 8: ") (flush) (read-line))))

(def set-position assoc)

(defn swap-player [user-symbol]
  (if (= "X" user-symbol) "O" "X"))

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

(defn next-player-turn [board user-symbol]
  (display-board board)
  (if (game-over? board)
    (println end-game)
    (next-player-turn (set-position board (get-user-position) user-symbol) (swap-player user-symbol))))

(defn start-game []
  (let [user-symbol (get-user-symbol)]
    (display-board numbered-board)
    (let [board (create-board)]
      (next-player-turn board user-symbol))))
