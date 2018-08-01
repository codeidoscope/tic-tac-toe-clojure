(ns tic-tac-toe-clojure.core)

(defn create-board []
  (into [] (repeat 9 nil)))
