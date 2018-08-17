(ns tic-tac-toe-clojure.core (:require [clojure.string :as string]))
(declare start-game)
(defn -main [& args]
  (println "Starting game")
  (start-game))

(defn create-board []
  (into [] (repeat 9 "_")))

(defn numbered-board []
  ["1" "2" "3" "4" "5" "6" "7" "8" "9"])

(defn build-board [board]
  (with-out-str (println (string/join "\n" (re-seq #".{1,3}" (apply str board))))))

(defn display-board [board]
  (print (build-board board)))

(defn get-user-symbol []
  (do (print "Please choose a symbol (X or O): ") (flush) (read-line)))

(defn get-user-position []
 (Integer/parseInt (do (print "Please choose a position between 1 and 9: ") (flush) (read-line))))

(defn set-position [board position user-symbol]
  (assoc board position user-symbol))

(defn swap-player [user-symbol]
  (if (= "X" user-symbol) "O" "X"))

(defn get-rows [board]
  (partition 3 board))

(defn get-columns [board]
  (apply map vector (get-rows board)))

(defn get-diagonals [board]
  [[(nth board 0) (nth board 4) (nth board 8)]
   [(nth board 2) (nth board 4) (nth board 6)]])

(defn join-sections [board]
  (concat (get-rows board) (concat (get-columns board)) (concat(get-diagonals board))))

(defn symbols-equal? [section]
  (if (and (= (nth section 0) (nth section 1) (nth section 2)) (not= (apply str section) "___")) true false))

(defn three-aligned? [board]
  (some true? (for [section (join-sections board)] (symbols-equal? section))))

(defn three-mapped? [board]
  (some true? (map symbols-equal? (join-sections board))))

(defn board-full? [board]
  (if (some #{"_"} board) false true))

(defn end-game []
  (with-out-str (println "Game is over")))

(defn game-over? [board]
  (if (or (three-aligned? board) (board-full? board)) true false))

(defn next-player-turn [board user-symbol]
 (display-board (set-position board (get-user-position) user-symbol))
 (swap-player user-symbol) board)

(defn take-turn [board user-symbol]
  (while (= (game-over? board) false) (do (next-player-turn board user-symbol))))

(defn start-game []
  (let [user-symbol (get-user-symbol)]
    (display-board (numbered-board))
    (let [board (create-board)]
      (if (game-over? (next-player-turn board user-symbol))
        (end-game)
        (next-player-turn board user-symbol)))))
