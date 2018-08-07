(ns tic-tac-toe-clojure.core (:require [clojure.string :as string]))
(defn -main [& args]
  (println "Working!"))

(def create-board
  (into [] (repeat 9 "_")))

(def numbered-board
  ["1" "2" "3" "4" "5" "6" "7" "8" "9"])

(defn display-board [board]
  (with-out-str (print (string/join "\n" (re-seq #".{1,3}" (apply str board))))))

(defn get-user-symbol []
  (do (print "Please choose a symbol (X or O): ") (flush) (read-line)))

(defn set-position [board position symbol]
  (assoc board position symbol))
