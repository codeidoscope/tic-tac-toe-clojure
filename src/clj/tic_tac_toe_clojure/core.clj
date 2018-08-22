(ns tic-tac-toe-clojure.core (:require [clojure.string :as string]))
(declare start-game)
(defn -main [& args]
  (println "Starting game")
  (start-game))

(defn create-board []
  (into [] (repeat 9 "_")))

(def numbered-board
  ["0" "1" "2" "3" "4" "5" "6" "7" "8"])

(defn build-board [board]
  (with-out-str (println (string/join "\n" (re-seq #".{1,3}" (apply str board))))))

(defn display-board [board]
  (print (build-board board)))

(defn get-player-type []
  (do (print "Please select an opponent (H for human or C for computer): ") (flush) (read-line)))

(defn get-user-symbol []
  (do (print "Please choose a symbol (X or O): ") (flush) (read-line)))

(defn get-human-position []
 (Integer/parseInt (do (print "Please choose a position between 0 and 8: ") (flush) (read-line))))

(defn get-computer-position [board randomiser]
  (get (randomiser (filter (fn [[_ marker]] (= "_" marker)) (map-indexed vector board))) 0))

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

(def end-game "Game is over")

(defn game-over? [board]
  (if (or (three-aligned? board) (board-full? board)) true false))

(defn next-player-turn [board user-symbol]
  (display-board board)
  (if (game-over? board)
    (println end-game)
    (next-player-turn (set-position board (get-human-position) user-symbol) (swap-player user-symbol))))

(defn take-turn [board user-symbol]
  (while (= (game-over? board) false) (do (next-player-turn board user-symbol))))

(defn start-game []
  (let [user-symbol (get-user-symbol)]
    (display-board numbered-board)
    (let [board (create-board)]
      (next-player-turn board user-symbol))))
