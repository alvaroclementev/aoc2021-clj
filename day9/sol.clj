;; Day 9
(require '[clojure.string :as str])

;; Utils
(defn split-lines [text]
  (str/split text #"\n"))

(defn process-line [line]
  (into [] (map #(Character/digit % 10) line)))

(defn read-input [path]
  "Read the input file"
  (->> path
      slurp
      str/trim
      split-lines
      (map process-line)
      (into [])))

(defn matrix-at [[x y] matrix]
  "Get the value at [x y] in matrix"
  (get-in matrix [y x]))

(defn adjacent-points [[x y] matrix]
  "Return the adjacent points to [x y] as
  [top right bottom left]"
  (let [rows (count matrix)
        cols (count (first matrix))
        top (if (> y 0) [x (dec y)])
        right (if (< x (dec cols)) [(inc x) y])
        bottom (if (< y (dec rows)) [x (inc y)])
        left (if (> x 0) [(dec x) y])]
    [top right bottom left]))

(defn adjacent-values [point matrix]
  "Return the values of the adjacent points to `point` as [top right bottom left],
  or `nil` if the adjacent-point is `nil`"
  (into [] (map #(matrix-at % matrix) (adjacent-points point matrix))))

(defn low-point? [point matrix]
  "Check if a position (x, y) is a low point in a matrix"
  (let [this (matrix-at point matrix)
        [top right bottom left] (adjacent-values point matrix)]
    (and
      ;; Check top position
      (or (nil? top) (< this top))
      ;; Check right position
      (or (nil? right) (< this right))
      ;; Check bottom position
      (or (nil? bottom) (< this bottom))
      ;; Check left position
      (or (nil? left) (< this left)))))

(defn risk-level [point matrix]
  (inc (matrix-at point matrix)))

(defn find-low-points [matrix]
  "Find all the low points in the `matrix`"
  (for [y (range (count matrix))
        x (range (count (first matrix)))
        :when (low-point? [x y] matrix)]
    [x y]))

(defn low-points-risk [matrix]
  "Find the risk level of all the low points in a matrix"
  (map #(risk-level % matrix) (find-low-points matrix)))


;; Tests
(read-input "sample.txt")
(first (read-input "sample.txt"))

(low-point? [0 0] [[1 2] [3 4]])
(low-point? [0 1] [[1 2] [3 4]])
(low-point? [1 0] [[1 2] [3 4]])
(low-point? [1 1] [[1 2] [3 4]])


;; Part 1
(defn solve1 [path]
  (->> path
       read-input
       low-points-risk
       (reduce +)))

(solve1 "sample.txt")
(solve1 "input.txt")

;; Part 2

;; NOTE(alvaro):
;; For this we are first assuming that no basin flows
;; into 2 different low points at the same time
(defn basin-candidate? [point from matrix]
  "Check if `point` is a candidate of the basin coming from `from`"
  (let [value (matrix-at point matrix)
        from-value (matrix-at from matrix)]
    (and (< value 9) (> value from-value))))

(defn next-basin-candidates [point basin matrix]
  "Find the next basin candidates from point"
  (->> 
    (adjacent-points point matrix)
    (remove nil?)
    (remove #(= point %))
    (remove #(contains? basin %))
    ;; We want the arguments flipped so that the `from`
    ;; is second
    (map #(vector %2 %1) (repeat point))))

(defn find-basin [low-point matrix]
  "Find all the points in `matrix` that flow to the low point at [x y]"
  ;; NOTE(alvaro): A candidate will be a pair of [point, from-point]
  (loop [basin #{low-point}
         candidates (map
                      vector
                      (remove nil? (adjacent-points low-point matrix))
                      (repeat low-point))]
    (if (empty? candidates)
      basin
      (let [[point from] (first candidates)]
        (if (basin-candidate? point from matrix)
          ;; This is part of this basin, add the new candidates to the head of
          (recur
            (conj basin point)
            ;; Add the new candidates
            (apply conj (rest candidates) (next-basin-candidates point basin matrix)))
          ;; Continue with the rest of the candidates
          (recur basin (rest candidates)))))))

(defn basin-size [low-point matrix]
  "Compute the size of the basin that flows to the
  `low-point` in `matrix`"
  (count (find-basin low-point matrix)))

;; Tests
(find-basin [1 0] (read-input "sample.txt"))
(find-basin [9 0] (read-input "sample.txt"))
(count (find-basin [9 0] (read-input "sample.txt")))
(count (find-basin [2 2] (read-input "sample.txt")))
(count (find-basin [6 4] (read-input "sample.txt")))

(defn solve2 [path]
  (let [matrix (read-input path)]
    (->> matrix
      find-low-points
      (map #(basin-size % matrix))
      ;; Sort in reverse order
      (sort (comp - compare))
      (take 3)
      (reduce *))))


(solve2 "sample.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
