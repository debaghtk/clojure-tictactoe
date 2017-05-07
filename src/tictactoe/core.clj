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
  (clojure.string/join "\n" (for [row board] (show-row row))))

(defn get-square
  [board x y]
  {:pre [(<= 0 x 2) (<= 0 y 2)]}
  (get-in board [x y]))

(defn check-rows
  [board v]
  (some true?
        (for [i (range 3)] (apply = v (get board i)))))

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
  (some true? ((juxt check-rows check-cols check-diagonals) board v)))

(defn draw?
  [board]
  (not
   (or (has-empty-square? board)
       (winner? board "X")
       (winner? board "O"))))

(declare number-of-empty-squares)
(declare next-boards)
(declare mark-square)

(defn evaluate
  [board v]
  (cond
    (winner? board v) (+ 10 (number-of-empty-squares board))
    (winner? board (opponent v)) (* -1 (+ 10 (number-of-empty-squares board)))
    (draw? board) 0
    :else nil))

(defn maximize
  [board v]
  (if (evaluate board v)
    (evaluate board v)
    (let [child-boards (next-boards board (opponent v))
          possible-scores (map #(* -1 (maximize % (opponent v))) child-boards)]
      (apply min possible-scores))))

(defn possible-scores
  [board v]
  (for [i (range 3) j (range 3)]
    (if-not (mark-square board i j v)
      nil
      (maximize (mark-square board i j v) v))))

(defn best-move
  [board v]
  (let [all-possible-scores (possible-scores board v)
        max-score (apply max (remove nil? all-possible-scores))
        min-score (apply min (remove nil? all-possible-scores))
        score (cond (< max-score (* -1 min-score)) max-score
                    :else max-score)
        index (.indexOf all-possible-scores score)]
    (cond
      (= index 0) [0 0]
      (= index 1) [0 1]
      (= index 2) [0 2]
      (= index 3) [1 0]
      (= index 4) [1 1]
      (= index 5) [1 2]
      (= index 6) [2 0]
      (= index 7) [2 1]
      (= index 8) [2 2])))

(defn mark-square
  [board x y v]
  {:pre [(<= 0 x 2)
         (<= 0 y 2)
         (or (= v "X") (= v "O") (= v " "))]}
  (when (= (get-square board x y) " ")
    (assoc-in board [x y] v)))

(defn game-over?
  [board]
  (or (winner? board "X")
      (winner? board "O")
      (not (has-empty-square? board))))

(defn number-of-empty-squares
  [board]
  (apply + (for [i (range 3)] ((frequencies (get board i)) " " 0))))

(defn whose-move?
  [board]
  (if (odd? (number-of-empty-squares board))
    "X"
    "O"))

(defn next-boards
  [board v]
  (when-not (game-over? board)
    (remove nil?
            (for [i (range 3) j (range 3)]
              (mark-square board i j v)))))

(defn game-tree
  [board v]
  {:board board :next (map #(game-tree % (opponent v)) (next-boards board v))})

(defn winning-path
  ([board v]
   (winning-path board v [board]))
  ([board v acc]
   (if (or (winner? board v) (draw? board))
     acc
     (let [gt (game-tree board v)
           possible-moves (filter #(not (winner? (:board %)  (opponent v))) (:next gt))]
       (first  (for [next-move possible-moves
                     :let [next-acc (conj acc next-move)
                           actual-acc (winning-path next-move v next-acc)]
                     :when (not= actual-acc next-acc)]
                 actual-acc))))))

(defn move
  [board v]
  (let [x (rand-int 3)
        y (rand-int 3)]
    (if-let [next-board (mark-square board x y v)]
      next-board
      (recur board v))))

(defn gen-random-board
  ([]
   (gen-random-board empty-board "X"))
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
