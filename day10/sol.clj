;; Day 10
(require '[clojure.string :as str])

;; Utils
(defn split-lines [text]
  (str/split text #"\n"))

(defn read-input [path]
  "Read the input file"
  (->> path
      slurp
      str/trim
      split-lines
      (into [])))


(defn matching? [this other]
  "Check if `this` is a character that matches `other` (that is, one that 'closes' the chunk opened by `other`)"
  (and (not (nil? this))
       (= this ({\) \(
                 \] \[
                 \} \{
                 \> \<} other))))

(defn opening? [chr]
  "Returns `true` if `chr` is an 'opening' character, that is, it opens a chunk"
  (contains? #{\( \[ \{ \<} chr))

(defn first-illegal-character [line]
  "Return the first illegal character in the `line` (or `nil`)"
  (loop [stack '()
         remaining line]
    (if (empty? remaining)
      ;; Al fine
      nil
      (let [chr (first remaining)]
        (if (opening? chr)
          ;; We start a new chunk
          (recur 
            (conj stack chr)
            (rest remaining))
          ;; We check that this is the matching character
          (if (matching? (first stack) chr)
            ;; Close the chunk
            (recur
              (rest stack)
              (rest remaining))
            ;; Found an illegal character
            chr))))))


(def illegal-score 
  {\) 3
   \] 57
   \} 1197
   \> 25137})


;; Tests
(read-input "sample.txt")
(matching? \( \))
(matching? \) \()
(matching? \e \()
(first-illegal-character (first (read-input "sample.txt")))

;; Part 1
(defn solve1 [path]
  (->> path
       read-input
       (keep first-illegal-character)
       (map illegal-score)
       (reduce +)))

(solve1 "sample.txt")
(solve1 "input.txt")

;; Part 2
(defn build-line-stack [stack chr]
  "Reducer function to build the stack"
  (if (opening? chr)
    (conj stack chr)
    (do 
      (assert (matching? (first stack) chr))
      (rest stack))))

(def match
  {\( \)
   \[ \]
   \{ \}
   \< \>})



(defn repair-characters [line]
  "Return the characters required to repair a line"
  (->> line
    ;; Build the stack of openers
    (reduce build-line-stack '())
    ;; Get the right ones
    (map match)))


(defn completion-score [chrs]
  "Compute the total completion score for a set of chrs"
  (reduce (fn [score chr]
            (+ (* 5 score) 
               ({\) 1
                 \] 2
                 \} 3
                 \> 4} chr))) 0 chrs))


(defn median [coll]
  "Get the median value of a collection of numbers"
  (let [sorted (sort coll)
        cnt (count coll)
        midpoint (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted midpoint)
      (let [bot-val (nth sorted midpoint)
            top-val (nth sorted (inc midpoint))]
        (/ (+ bot-val top-val) 2)))))


;; Tests
(->> "sample.txt"
     read-input
     ((comp nil? first-illegal-character) filter)
     first
     repair-characters
     (apply str))



;; Part 2
(defn solve2 [path]
  (->> path
       read-input
       ;; Keep the ones that are actually OK
       (filter (comp nil? first-illegal-character))
       ;; Get the repair characters
       (map repair-characters)
       (map completion-score)
       (into [])
       median))


(solve2 "sample.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
