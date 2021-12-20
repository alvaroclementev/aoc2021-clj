;; Day 5
(require '[clojure.string :as str])

;; Utils
(defn read-input [path]
  "Read the input file"
  (->> path
      slurp
      (#(str/split % #"\n"))
      (map parse-line)))

(defn parse-line [line]
  "Parse the line description 'x1, y1 -> x2, y2', returning the [[x1 y1] [x2 y2]]"
  (let [coords (map #(Integer/parseInt %) (str/split line #"\D+"))
        [x1 y1 x2 y2] coords]
    [[x1 y1] [x2 y2]]))

(defn get-x [point]
  (first point))

(defn get-y [point]
  (second point))

(defn find-matrix-dims [lines]
  "Find the minimum shape required to draw a diagram for `lines`"
  (let [xs (concat (map #(get-x (first %)) lines) (map #(get-x (second %)) lines))
        ys (concat (map #(get-y (first %)) lines) (map #(get-y (second %)) lines)) ]
    [(inc (apply max xs)) (inc (apply max ys))]))

(defn build-matrix [cols rows]
  "Return a matrix of size cols x rows"
  (into [] (repeatedly rows #(into [] (repeat cols 0)))))


(defn vertical? [from to]
  (= (get-x from) (get-x to)))

(defn horizontal? [from to]
  (= (get-y from) (get-y to)))

(defn draw-vertical-line [diag from to]
  "Draw a vertical line in `diag` from `from` to `to`"
  (let [col (get-x from)
        y1 (get-y from)
        y2 (get-y to)
        rows (if (< y1 y2) (range y1 (inc y2)) (range y1 (dec y2) -1))]
    (reduce #(assoc %1 %2 (update (nth %1 %2) col inc)) diag rows)))

(defn draw-horizontal-line [diag from to]
  "Draw a horizontal line in `diag` from `from` to `to`"
  (let [row (get-y from)
        x1 (get-x from)
        x2 (get-x to)
        cols (if (< x1 x2) (range x1 (inc x2)) (range x1 (dec x2) -1))]
    (assoc
      diag
      row
      (into [] (reduce #(update %1 %2 inc) (nth diag row) cols)))))


(defn draw-diagonal-line [diag from to]
  "Draw a vertical line in `diag` from `from` to `to`"
  (let [[x1 y1] from
        [x2 y2] to
        cols (if (< x1 x2) (range x1 (inc x2)) (range x1 (dec x2) -1))
        rows (if (< y1 y2) (range y1 (inc y2)) (range y1 (dec y2) -1))
        coords (map list rows cols)]
    (reduce
      #(assoc %1 (first %2) (update (nth %1 (first %2)) (second %2) inc))
      diag
      coords)))


(defn draw-line [diag line]
  "Draw a line in `diag` from `from` to `to`, increasing the counters in all the positions"
  (let [[from to] line]
    (cond
      (vertical? from to)
      ;; Draw vertical line
      (draw-vertical-line diag from to)
      (horizontal? from to)
      ;; Draw vertical line
      (draw-horizontal-line diag from to)
      ;; Else we assume it's diagonal
      :else
      (draw-diagonal-line diag from to))))


;; Part 1
(defn solve1 [path]
  (let [lines (read-input path)
        filtered-lines (filter #(or (apply horizontal? %) (apply vertical? %)) lines)
        [cols rows] (find-matrix-dims filtered-lines)
        diagram (build-matrix cols rows)
        final-diagram (reduce draw-line diagram filtered-lines)]
    (count (filter #(> % 1) (flatten final-diagram)))))


(solve1 "sample.txt")
(solve1 "input.txt")


;; Part 2
(defn solve2 [path]
  (let [lines (read-input path)
        [cols rows] (find-matrix-dims lines)
        diagram (build-matrix cols rows)
        final-diagram (reduce draw-line diagram lines)]
    (count (filter #(> % 1) (flatten final-diagram)))))

(solve2 "sample.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
