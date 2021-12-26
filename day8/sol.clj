;; Day 8
(require '[clojure.string :as str])

;; Utils
(defn split-lines [text]
  (str/split text #"\n"))

(defn process-line [line]
  (map #(str/split % #" ") (str/split line #" \| ")))

(defn read-input [path]
  "Read the input file"
  (->> path
      slurp
      str/trim
      split-lines
      (map process-line)))

(first (read-input "sample.txt"))

;; Number tests
(defn is-one? [pattern]
  (= (count pattern) 2))

(defn is-four? [pattern]
  (= (count pattern) 4))

(defn is-seven? [pattern]
  (= (count pattern) 3))

(defn is-eight? [pattern]
  (= (count pattern) 7))

(defn which-character [pattern]
  "Finds which character corresponds to the pattern, or nil"
  (cond 
    (is-one? pattern) "1"
    (is-four? pattern) "4"
    (is-seven? pattern) "7"
    (is-eight? pattern) "8"))


;; Part 1
(defn solve1 [path]
  (->> path
      read-input
      (map second)
      flatten
      (filter which-character)
      count))

(solve1 "sample.txt")
(solve1 "input.txt")

;; Part 2

;; Represent the segments with this nomenclature
;;  aaaa 
;; b    c
;; b    c
;;  dddd 
;; e    f
;; e    f
;;  gggg 
;; 
;; The target pattern will be represented in a vector 
;; as "abcdefg"

(def pattern->number 
  {"abcefg" 0
   "cf" 1
   "acdeg" 2
   "acdfg" 3
   "bcdf" 4
   "abdfg" 5
   "abdefg" 6
   "acf" 7
   "abcdefg" 8
   "abcdfg" 9})


(defn build-guess [])


(some nil? (map {\a \b} "abc"))
(some nil? (map {\a \b 
                 \b \d
                 \c \d} "abc"))

{\a \b}


(defn solve2 [path]
  (let [crabs (read-input path)
        max-crab (apply max crabs)]
    (apply min (map #(position-cost-2 % crabs) (range (inc max-crab))))))

(solve2 "sample.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
