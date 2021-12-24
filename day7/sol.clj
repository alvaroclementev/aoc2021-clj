;; Day 7
(require '[clojure.string :as str])

;; Utils
(defn read-input [path]
  "Read the input file"
  (->> path
      slurp
      str/trim
      process-line))

(defn process-line [line]
  (into [] (map #(Integer/parseInt %) (str/split line #","))))


(defn argmin [coll]
  "Find the index of the minimum value in a collection"
  (second (apply min-key first (map vector coll (range)))))


;; Part 1
(defn position-cost [position crabs]
  "Compute the cost of aligning the `crabs` at `position`"
  (reduce + 0 (map #(Math/abs (- % position)) crabs)))


(defn solve1 [path]
  (let [crabs (read-input path)
        max-crab (apply max crabs)]
    (apply min (map #(position-cost % crabs) (range (inc max-crab))))))

(solve1 "sample.txt")
(solve1 "input.txt")

;; Part 2
(defn gauss-sum [n]
  "Result of suming 1 + 2 + ... + n"
  (/ (* n (inc n)) 2))

(defn position-cost-2 [position crabs]
  "Compute the cost of aligning the `crabs` at `position`"
  (reduce + 0 (map #(gauss-sum (Math/abs (- % position))) crabs)))

(defn solve2 [path]
  (let [crabs (read-input path)
        max-crab (apply max crabs)]
    (apply min (map #(position-cost-2 % crabs) (range (inc max-crab))))))

(solve2 "sample.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
