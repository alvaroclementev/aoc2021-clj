;; Day 4
(require '[clojure.string :as str])

;; Utils
(defn read-input [path]
  "Read the input file"
  (->> path
      slurp
      (#(str/split % #"\n"))))

(defn debug [f]
  "Log a function call and its result"
  (fn [& args]
    (let [res (apply f args)
          _ (println "Called with " args " -> " res)]
      res)))

(defn debug-reduce [f start coll]
  (reduce (debug f) start coll))


(defn process-line [boards line]
  "Process the lines of the input"
  (if (empty? (str/trim line))
    ; Start of a new board
    (conj boards [])
    ; Append a new line to the last board
    (let [current-board (last boards)
          rest-boards (into [] (butlast boards))
          line-nums (str/split (str/trim line) #" +")]
      (conj rest-boards
            (conj current-board
                  (into [] (map #(Integer/parseInt %) line-nums)))))))

(defn parse-boards [lines]
  "Parse the bingo boards from the input lines. Boards are separated by empty lines"
  (into [] (reduce process-line [] lines)))

(defn process-input [input]
  [(into [] (map #(Integer/parseInt %) (str/split (first input) #",")))
   (parse-boards (rest input))])


(defn transpose [rows]
  "Transpose a matrix (list of rows)"
  (into [] (apply map vector rows)))


(def sample-board (let [[nums boards] (process-input (read-input "sample.txt"))]
  (first boards)))


;; NOTE(alvaro): A checked number is marked with `-1`, which should be an invalid value
(defn row-finished? [board]
  "Checks if this board has any rows fully marked with `-1`"
  (some #(= (reduce + 0 %) -5) board))

(defn col-finished? [board]
  "Checks if this board has any columns fully marked with `-1`"
  (some #(= (reduce + 0 %) -5) (transpose board)))


(defn board-finished? [board]
  "Checks if this board has finished"
  (or (row-finished? board) (col-finished? board)))


(defn find-num [num board]
  "Find the position [x y] of `num` in board, else return `nil`"
  (let [row-idxs (map #(.indexOf % num) board)]
    (first (keep-indexed #(when-not (= %2 -1) [%2 %1]) row-idxs))))


(defn mark-number [num board]
  "Mark the number `num` in `board` with a `-1` in its position"
  (let [pos (find-num num board)
        [x y] pos]
    (if pos (assoc board y (assoc (nth board y) x -1))
      board)))

(defn compute-score [num board]
  "Compute the score associated with this board"
  (* num (reduce + 0 (filter #(>= % 0) (flatten board)))))


(defn winning-move [nums board]
  "Find at which move will the number end. Returns [counter last-num winner-board]"
  (loop [counter 0
         rem-nums nums
         marked-board board]
    (if (empty? rem-nums) nil
      (let [next-num (first rem-nums)
            new-board (mark-number next-num marked-board)]
        (if (board-finished? new-board)
          ;; We have a winner
          [counter next-num new-board]
          (recur (inc counter) (rest rem-nums) new-board))))))


;; Part 1
(defn solve1 [path]
  (let [[nums boards] (process-input (read-input path))
        winning-moves (map #(winning-move nums %) boards)
        sorted-winners (sort-by first winning-moves)
        ;; Pick the actual winner
        [counter last-num winner] (first sorted-winners)]
    (compute-score last-num winner)))


(solve1 "sample.txt")
(solve1 "input.txt")


;; Part2
(defn solve2 [path]
  (let [[nums boards] (process-input (read-input path))
        winning-moves (map #(winning-move nums %) boards)
        sorted-winners (sort-by first winning-moves)
        ;; Pick the actual winner
        [counter last-num winner] (last sorted-winners)]
    (compute-score last-num winner)))


(solve2 "sample.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
