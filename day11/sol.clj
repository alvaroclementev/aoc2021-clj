;; Day 11
(require '[clojure.string :as str])

;; Utils
(defn split-lines [text]
  (str/split text #"\n"))

(defn process-line [line]
  (->> line
    str/trim
    (map #(Character/digit % 10))
    (into [])))

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

(defn update-matrix-at [[x y] f matrix]
  "Get the value at [x y] in matrix"
  (update-in matrix [y x] f))


(defn adjacent-points [[x y] matrix]
  "Return the adjacent points to [x y] as the non-nil values from 
  [top right bottom left top-right bottom-right bottom-left top-left]"
  (let [rows (count matrix)
        cols (count (first matrix))
        ;; Useful conditionals
        has-top (> y 0)
        has-right (< x (dec cols))
        has-bottom (< y (dec rows))
        has-left (> x 0)
        ;; Actual adjancents
        top (when has-top [x (dec y)])
        right (when has-right [(inc x) y])
        bottom (when has-bottom [x (inc y)])
        left (when has-left [(dec x) y])
        ;; Diagonals
        top-right (when (and has-top has-right) [(inc x) (dec y)])
        bottom-right (when (and has-bottom has-right) [(inc x) (inc y)])
        bottom-left (when (and has-bottom has-left) [(dec x) (inc y)])
        top-left (when (and has-top has-left) [(dec x) (dec y)])]
    (->>
      [top right bottom left top-right bottom-right bottom-left top-left]
      (keep identity)
      (into []))))


(defn point-step [point matrix flash-matrix]
  "Process the effects of the update of a single point in the matrix"
  (let [prev-value (matrix-at point matrix)
        prev-flashed (matrix-at point flash-matrix)
        next-value (inc prev-value)
        flashed (and (> next-value 9) (not prev-flashed))]
    (if flashed
      [(update-matrix-at point #(* 0 %) matrix)
       (update-matrix-at point #(or true %) flash-matrix)]
      [(update-matrix-at point #(if prev-flashed % (inc %)) matrix)
       flash-matrix])))


(defn create-flash-matrix
  "Create a matrix with the same shape as `matrix` but all false"
  ([matrix] (create-flash-matrix matrix false))
  ([matrix init-value] (let [rows (count matrix)
                             cols (count (first matrix))]
                         (into [] (repeatedly rows #(into [] (repeat cols init-value)))))))


(defn all-points [matrix]
  "Get a list of pairs of all coordinates in the "
  (into [] (for [y (range (count matrix))
                 x (range (count (first matrix)))]
             [x y])))


(defn step 
  "Run a step in a matrix Returns [flashes new-matrix]."
  ([matrix] (step 0 matrix))
  ([flashes matrix]
   (loop [flashes flashes
          update-queue (all-points matrix)
          matrix matrix
          flash-matrix (create-flash-matrix matrix)
          iterations 0]
     (assert (< iterations 100000))
     (if (empty? update-queue)
       [flashes matrix]
       ;; Run an update step on the next point
       (let [point (first update-queue)
             prev-value (matrix-at point matrix)
             [next-matrix next-flash-matrix] (point-step point matrix flash-matrix)
             next-value (matrix-at point next-matrix)
             has-flashed (< next-value prev-value)]
         (if has-flashed
           ;; Mark its neighbours to update
           (recur (inc flashes)
                  (apply conj (rest update-queue) (adjacent-points point next-matrix))
                  next-matrix
                  next-flash-matrix
                  (inc iterations))
           ;; Just continue with the next point
           (recur flashes
                  (rest update-queue)
                  next-matrix
                  next-flash-matrix
                  (inc iterations))))))))


(defn simulate-steps [n matrix]
  "Run the simulation for `n` steps"
  (->> matrix
       (conj [0])
       (iterate #(apply step %))
       (take (inc n))
       last))


;; Part 1
(defn solve1 [path]
  (->> path
       read-input
       (simulate-steps 10)
       first))

(solve1 "sample.txt")
(solve1 "input.txt")


;; Part 2
(defn synchronized? [matrix]
  "Returns true if all the members of the matrix are equal"
  (every? #(apply = %) matrix))

(defn simulate-until [pred matrix]
  "Run the simulation until `pred` is true"
  (->> matrix
       (conj [0])
       (iterate #(apply step %))
       (take-while #(not (pred (second %))))))

(defn solve2 [path]
  (->> path
       read-input
       (simulate-until synchronized?)
       count))


(solve2 "sample.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
