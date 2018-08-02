(ns tic-tac-toe-clojure.core)

(defn create-board []
  (into [] (repeat 9 "_")))

(defn display-board []
  (with-out-str (println (clojure.string/join "\n" (re-seq #".{1,3}" (apply str (create-board)))))))
