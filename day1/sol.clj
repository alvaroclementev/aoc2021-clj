;; Day 1
(require '[clojure.string :as str])

;; Utils
(defn read-input [path]
  "Read the input file"
  (->> path
      slurp
      (#(str/split % #"\n"))
      (map #(Integer/parseInt %))))

(defn pairwise [lst]
  (map vector lst (rest lst)))

(defn triplewise [lst]
  (map vector 
       lst 
       (rest lst)
       (rest (rest lst))))

;; TODO(alvaro): I should be able to build a macro to generalize this!
(defmacro n-wise 
  "Iterate over a collection using a window of size `n`"
  ; (map vector (k k-1 k-2 ... k-(n-1)))
  [n lst]
  ())


;; Part 1
(def input (read-input "input-test.txt"))
(def input (read-input "input.txt"))

(defn solve1 [path]
  (->> path
       read-input
       pairwise
       (filter (fn [[prv nxt]] (> nxt prv)))
       count))

(solve1 "input-test.txt")
(solve1 "input.txt")



;; Part 2
(defn solve2 [path]
  (->> path 
       read-input
       triplewise
       (map #(apply + %))
       pairwise
       (filter (fn [[prv nxt]] (> nxt prv)))
       count))

(solve2 "input-test.txt")
(solve2 "input.txt")


;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
