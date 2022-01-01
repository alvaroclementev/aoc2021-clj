; Day 15
(require
  '[clojure.string :as str]
  '[clojure.data.priority-map :refer [priority-map-keyfn]])

(defn debug [x]
  "Simple debug function to insert in the middle of a thread macro"
  (println x)
  x)

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


;; A candidate contains:
;;   - point: the coordinates in the matrix
;;   - value: the value of that position in the matrix
;;   - from: the best known
(defn build-node
  ([point value]
   (build-node point value nil ##Inf))
   ([point value from distance]
    {:point point
     :value value
     :from nil
     :distance distance}))


(defn init-unvisited [matrix from]
  "Create a priority map of [x y] -> Node
  (sorted by :distance) of all the unvisited nodes left in matrix"
  ;; We make sure the first node (distance 0) is the `from`, which is the
  ;; starting point of Dijsktra's algorithm
  (assoc
    (into
      (priority-map-keyfn :distance)
      (for [y (range (count matrix))
            x (range (count (first matrix)))
            :let [point [x y]]
            :when (not= point from)]
        [point (build-node [x y] (matrix-at [x y] matrix))]))
    from
    (build-node from 0 nil 0)))

(defn build-path [to-node visited]
  "Builds an array of the nodes visited in the shortest path to get to `to-node`"
  (reverse 
    (take-while 
      (complement nil?) 
      (iterate #(visited (:from %)) to-node))))

(defn dijkstra
  "Dijkstra's algorithm for shortest path finding"
  ([matrix]
   (dijkstra matrix
             [0 0]
             [(dec (count (first matrix)))
              (dec (count matrix))]))
  ([matrix from to]
   (loop [unvisited (init-unvisited matrix from)
          ;; A map to keep the visited nodes for debug purposes
          visited {}]
     (if (empty? unvisited)
       ;; We failed to find a path to `to`
       nil
       ;; We perform a step of the algorithm
       (let [[point node] (first unvisited)]
         (if (= point to)
           ;; We are visiting the destination node, so that means we are done
           ; (build-path node visited)
           node
           ;; Or else we run a regular step of the algorithm
           (let [neigh-points (adjacent-points point matrix)
                 neighbors (keep unvisited neigh-points)
                 ;; Check the new distances of these neighbors
                 new-neighbors-distances (map #(vector % (+ (:distance node) (:value %))) neighbors)
                 ;; Find the new distances that are smaller than the old ones
                 to-update-neighbors (keep #(let [[neighbor new-dist] %]
                                              (when (< new-dist (:distance neighbor))
                                                (build-node (:point neighbor)
                                                            (:value neighbor)
                                                            point
                                                            new-dist))) new-neighbors-distances)]
             ;; Remove the visited node and update the relevant neighbors
             (recur
               (reduce #(assoc %1 (first %2) (second %2))
                       (dissoc unvisited point)
                       (map #(vector (:point %) %) to-update-neighbors))
               (assoc visited point node)))))))))



;; Test
(dijkstra (read-input "sample.txt"))

;; Part 1
(defn solve1 [path]
  (->> path
       read-input
       dijkstra
       :distance))

(solve1 "sample.txt")
(solve1 "input.txt")

;; Part 2
(defn maybe-wrap-value [x y v rows cols]
  (let [wraps-x (quot x cols)
        wraps-y (quot y rows)
        wraps (+ wraps-x wraps-y)
        wrapped-value (+ v wraps)]
      (+ (rem wrapped-value 10) (quot wrapped-value 10))))

(defn wrapped [n matrix]
  "Wrap a matrix `n` times to the right and to the left, 
  increasing (and wrapping) the values each time"
  (let [rows (count matrix)
        cols (count (first matrix))
        wrapped-rows (* n rows)
        wrapped-cols (* n cols)]
    (into []
          (map #(into [] %)
               (partition wrapped-cols
                          (for [y (range wrapped-rows)
                                x (range wrapped-cols)
                                :let [mat-y (rem y rows)
                                      mat-x (rem x cols)
                                      v (matrix-at [mat-x mat-y matrix] matrix)]]
                            (maybe-wrap-value x y v rows cols)))))))

;; Tests
(=
 (wrapped 5 (read-input "sample.txt"))
 (read-input "sample-wrapped.txt"))

(defn solve2 [path]
  (->> path
       read-input
       (wrapped 5)
       dijkstra
       :distance))


(solve2 "sample.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
