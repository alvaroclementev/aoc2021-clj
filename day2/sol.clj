;; Day 2
(require '[clojure.string :as str])

;; Utils
(defn read-input [path]
  "Read the input file"
  (->> path
      slurp
      (#(str/split % #"\n"))))


;; The position here is a pair [horizontal depth]

;; Part 1
(defn update-position
  "Update the position of the submarine based the instruction"
  [current-position [direction amount]]
  (({"forward" #(vector (+ (first %1) %2)
                        (second %1))
     "down" #(vector (first %1)
                        (+ (second %1) %2))
     "up" #(vector (first %1)
                        (- (second %1) %2))} direction)
   current-position amount))

;; Testing the updates
(update-position [0 0] '("forward" 5))
(update-position [0 0] '("up" 5))
(update-position [0 0] '("down" 5))

(defn solve1 [path]
  (->> path
    read-input
    (map #(str/split % #" "))
    (map #(vector (first %) (Integer/parseInt (second %))))
    (reduce update-position [0 0])
    (reduce *)))

(solve1 "input-test.txt")
(solve1 "input.txt")

;; Part 2
;; The position includes now a third component, the `aim`
(defn update-position-aim
  "Update the position (and aim) of the submarine based the instruction"
  [current-position [direction amount]]
  (({"forward" #(let [[hor depth aim] %1]
                  [(+ hor %2) (+ depth (* aim %2)) aim])
     "down" #(let [[hor depth aim] %1]
               [hor depth (+ aim %2)])
     "up" #(let [[hor depth aim] %1]
               [hor depth (- aim %2)])}
    direction)
   current-position amount))

;; Test the new update function
(update-position-aim [0 0 0] '("forward" 5))
(update-position-aim [0 0 0] '("up" 5))
(update-position-aim [0 0 0] '("down" 5))
(update-position-aim [5 0 5] '("forward" 5))

(defn solve2 [path]
  (->> path
    read-input
    (map #(str/split % #" "))
    (map #(vector (first %) (Integer/parseInt (second %))))
    (reduce update-position-aim [0 0 0])
    ;; We don't want to multiply the aim
    butlast
    (reduce *)))

(solve2 "input-test.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
