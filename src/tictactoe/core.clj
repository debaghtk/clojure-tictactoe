(ns tictactoe.core
  (:gen-class))

(def empty-board [[" " " " " "] [" " " " " "] [" " " " " "]])

(defn has-empty-square?
  [board]
  (some #(= " " %) (flatten board)))

(def opponent {"X" "O" "O" "X"})

(defn transpose-board
  [board]
  (apply mapv vector board))

(defn show-row
  [row]
  (str "| " (clojure.string/join " | " row) " |"))

(defn show-board
  [board]
  (clojure.string/join "\n" (for [row board] (show-row row))) )

(defn get-square
  [board x y]
  {:pre [(<= 0 x 2) (<= 0 y 2)]}
  (get-in board [x y]))

(defn check-rows
  [board v]
  (some true? (for [i (range 3)] (apply = v (get board i)))))

(defn check-cols
  [board v]
  (some true?
        (for [i (range 3)]
          (apply = v
                 (for [j (range 3)] (get-in board [j i]))))))

(defn check-diagonals
  [board v]
  (or
    (apply = v (for [i (range 3)] (get-in board [i i])))
    (apply = v (for [i (range 3)] (get-in board [i (- 2 i)])))))

(defn winner?
  [board v]
  (some true? ((juxt check-rows check-cols check-diagonals) board v)) )

(defn draw?
  [board]
  (not
    (or (has-empty-square? board)
        (winner? board "X")
        (winner? board "O"))))

(defn mark-square
  [board x y v]
  {:pre [(<= 0 x 2)
         (<= 0 y 2)
         (or (= v "X") (= v "O")(= v " "))]}
  (when (= (get-square board x y) " ")
    (assoc-in board [x y] v)))

(defn game-over?
  [board]
  (or (winner? board "X")
      (winner? board "O")
      (not (has-empty-square? board))))

(defn move
  [board v]
  (let [x (rand-int 3)
        y (rand-int 3)]
    (if-let [next-board (mark-square board x y v)]
      next-board
      (recur board v))))

(defn gen-random-board
  ([]
   (gen-random-board empty-board (rand-nth (keys opponent))))
  ([board v]
   (if-not (has-empty-square? board)
     board
     (let [x (rand-int 3)
           y (rand-int 3)]
       (if-let [next-board (mark-square board x y v)]
         (recur next-board (opponent v))
         (recur board v))))))

(defn game
  ([]
   (game empty-board "X"))
  ([board v]
   (println (show-board board))
   (println " ")
   (if-not (game-over? board)
     (recur (move board v) (opponent v))
     [(winner? board "X") (winner? board "O") (draw? board)])))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


