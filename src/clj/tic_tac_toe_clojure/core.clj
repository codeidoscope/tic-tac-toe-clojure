(ns tic-tac-toe-clojure.core
  (:require [clojure.string :as string]))

(declare start-game)
(declare scored-moves)
(declare score-move)
(declare get-rows)

(defn -main [& args]
  (println "Starting game / Partie commencée")
  (start-game))

(defn get-square [number]
  (* number number))

(defn get-square-root [board]
  (int (Math/sqrt (count board))))

(defn create-board [size]
  (into [] (repeat (get-square size) "_")))

(defn numbered-board [size]
  (map str (range (get-square size))))

(defn insert-pipe-sign [board]
  (let [rows (get-rows board)]
    (map #(interpose " | " %) rows)))

(defn generate-divider [board]
  (let [piped-rows (insert-pipe-sign board)]
    (str "\n" (string/join (repeat (count (apply str (nth piped-rows 0))) "-")) "\n")))

(defn insert-dividers [board]
  (interpose (generate-divider board) (insert-pipe-sign board)))

(defn format-board [board]
    (string/join (flatten (insert-dividers board))))

(defn display-board [board]
  (print (format-board board) "\n\n"))

(defn select-position [board]
  {:en (str "Please choose a position between " (first board) " and " (last board)": ")
   :fr (str "Choisissez une position entre " (first board) " et " (last board)": ")})

(def occupied-position
  {:en "This position is occupied, please select another: "
   :fr "Cette case est déjà occupée, choisissez en une autre: "})

(defn invalid-position-selection [board]
  {:en (str "This number is invalid, please enter a number between "(first board)" and "(last board)": ")
   :fr (str "Ce nombre n'est pas valide, choisissez un nombre entre "(first board)" et "(last board)": ")})

(def select-board-size
  {:en "Please enter a number greater than 0 to determine the size of your board: "
   :fr "Choisissez un nombre plus large que 0 pour déterminer la taille de la grille: "})

(def invalid-board-size
  {:en "This number is invalid, pleaser enter a number greater than 0: "
   :fr "Ce nombre n'est pas valide, choisissez un nombre plus large que 0: "})

(defn valid-position-selection? [input board]
  (boolean (some #{input} board)))

(defn position-empty? [board position]
  (= (nth board position) "_"))

(defn prompt-user [prompt]
  (do (print prompt) (flush) (read-line)))

(def select-language-prompt "Please select a language / Choisissez une langue (EN / FR): ")

(def invalid-language-selection "Not valid, try again / Non valide, réessayez: ")

(defn valid-language-selection? [input]
 (boolean (some #{input} ["EN" "en" "En" "eN" "FR" "fr" "Fr" "fR"])))

(defn get-translated-prompt [prompt language]
 (get prompt language))

(defn select-language [prompt]
  (let [language (prompt-user prompt)]
    (if (valid-language-selection? language)
      (keyword (string/lower-case language))
      (select-language invalid-language-selection))))

(defn pick-board-size [prompt language]
  (let [user-input (prompt-user (get-translated-prompt prompt language))]
    (if (boolean (re-seq #"^[1-9]+[0-9]*$" user-input))
        (Integer/parseInt user-input)
        (pick-board-size invalid-board-size language))))

(defn get-human-position [board numbered-board prompt language]
  (let [user-input (prompt-user (get-translated-prompt prompt language))]
      (if (valid-position-selection? user-input numbered-board)
          (if (position-empty? board (Integer/parseInt user-input))
                (Integer/parseInt user-input)
                (get-human-position board numbered-board occupied-position language))
          (get-human-position board numbered-board (invalid-position-selection numbered-board) language))))

(defn find-empty-spots [board]
  (filter (fn [[_ marker]] (= "_" marker)) (map-indexed vector board)))

(def set-position assoc)

(def select-first-player
  {:en "Please select the first player to take a turn (H/h for human or C/c for computer): "
   :fr "Choisissez le premier joueur (H/h pour humain ou O/o pour ordinateur): "})

(def select-opponent
  {:en "Please select an opponent: "
   :fr "Choisissez un adversaire: "})

(def invalid-player-selection
  {:en "Invalid choice, please choose H/h for human or C/c for computer: "
   :fr "Ce choix n'est pas valide, choisissez H/h pour humain ou O/o pour ordinateur: "})

(defn valid-player-selection? [input]
  (boolean (some #{input} ["C" "c" "O" "o" "h" "H"])))

(defn get-rows [board]
  (let [size (get-square-root board)]
    (partition size board)))

(defn get-left-diagonal [size]
  (reduce (fn [a b] (conj a (+' (last a) (+ 1 size)))) [0 (+ 1 size)] (range (- size 2))))

(defn get-right-diagonal [size]
  (next (drop-last (reduce (fn [a b] (conj a (+' (last a) (- size 1)))) [(- size 1) (- size 1)] (range size)))))

(defn get-columns [board]
  (apply map vector (get-rows board)))

(defn- symbols-at [board positions]
  (map (fn [position] (nth board position)) positions))

(defn get-diagonals [board]
  (let [size (get-square-root board)]
    [(symbols-at board (get-left-diagonal size))
    (symbols-at board (get-right-diagonal size))]))

(defn join-sections [board]
  (concat (get-rows board) (concat (get-columns board)) (concat (get-diagonals board))))

(defn symbols-equal? [section symbol]
  (every? #{symbol} section))

(defn n-aligned? [board symbol]
  (some true? (for [section (join-sections board)] (symbols-equal? section symbol))))

(defn board-full? [board]
  (if (some #{"_"} board) false true))

(def end-game
  {:en "Game is over"
   :fr "Jeu terminé"})

(defn game-over? [board symbol1 symbol2]
  (or (n-aligned? board symbol1) (n-aligned? board symbol2) (board-full? board)))

(defn get-empty-spots [board]
  (remove #{"_"} (flatten (find-empty-spots board))))

(def start-depth 0)

(def max-depth 4)

(defn evaluate-board [board current-player opponent depth]
  (cond
    (= depth max-depth) 0
    (n-aligned? board current-player) 10
    (n-aligned? board opponent) -10
    :else 0))

(defn minimax [board maximising-player minimising-player depth]
  (if (or (= depth max-depth) (game-over? board maximising-player minimising-player))
    (evaluate-board board maximising-player minimising-player depth)
        (* -1 (val (apply max-key val (scored-moves board minimising-player maximising-player (inc depth)))))))

(defn score-move [board position current-player opponent depth]
  {position (minimax (set-position board position current-player) current-player opponent depth)})

(defn scored-moves [board current-player opponent depth]
  (let [empty-spots (get-empty-spots board)
        size (get-square-root board)]
    (into (sorted-map) (map #(score-move board % current-player opponent depth) empty-spots))))

(defn choose-best-move [board current-player opponent depth]
  (key (apply max-key val (scored-moves board current-player opponent depth))))

(defn get-computer-position [board numbered-board current-player opponent depth]
  (choose-best-move board current-player opponent depth))

(defprotocol Player
  (get-symbol [this])
  (get-move [this board numbered-board opponent language]))

(deftype HumanPlayer [symbol]
  Player
  (get-symbol [this] symbol)
  (get-move [this board numbered-board opponent language]
    (get-human-position board numbered-board (select-position numbered-board) language)))

(defn make-human-player [symbol] (HumanPlayer. symbol))

(deftype ComputerPlayer [symbol]
  Player
  (get-symbol [this] symbol)
  (get-move [this board numbered-board opponent language]
    (get-computer-position board numbered-board symbol (get-symbol opponent) start-depth)))

(defn make-computer-player [symbol] (ComputerPlayer. symbol))

(defn next-player-turn [board current-player opponent language]
  (display-board board)
  (if (game-over? board (get-symbol opponent) (get-symbol current-player))
    (println (get-translated-prompt end-game language))
    (recur
      (set-position board
                    (get-move current-player board (numbered-board (get-square-root board)) opponent language)
                    (get-symbol current-player))
      opponent
      current-player
      language)))

(defn get-player [marker prompt language]
  (let [player-type (prompt-user (get-translated-prompt prompt language))]
  (if (valid-player-selection? player-type)
      (if (boolean (some #{player-type} ["H" "h"]))
        (HumanPlayer. marker)
        (ComputerPlayer. marker))
        (get-player marker (get-translated-prompt invalid-player-selection language) language))))

(defn start-game []
  (let [language (select-language select-language-prompt)]
    (let [board-size (pick-board-size select-board-size language)]
      (let [player-1 (get-player "X" select-first-player language)
            player-2 (get-player "O" select-opponent language)]
        (display-board (numbered-board board-size))
        (next-player-turn (create-board board-size) player-1 player-2 language)))))
