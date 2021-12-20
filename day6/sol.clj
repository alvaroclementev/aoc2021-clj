;; Day 6
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

(defn next-state [state]
  "Get the next state of a given fish"
  (if (= state 0)
    ;; Reset the state to 6
    6
    ;; Decrement the state
    (dec state)))

(defn discard-second [f]
  "Return a function that calls `f` with just the first parameter"
  (fn [fst _]
    (f fst)))

(defn simulate-fish [population fish]
  "Simulate the behaviour of a given fish state"
  (let [new-state (next-state fish)
        next-pop (conj population new-state)]
    (if (> new-state fish)
      (conj next-pop 8)
      next-pop)))

(defn simulate-day [population]
  "Simulate a single day"
  (reduce simulate-fish [] population))


(defn simulate-n-days [simulation-fn n population]
  "Run the simulation over the `population` `n` times"
  (reduce (discard-second simulation-fn) population (range n)))

;; Tests
(read-input "sample.txt")
(simulate-n-days 10 [1 2 3])
(simulate-n-days 18 (read-input "sample.txt"))


;; Part 1
(defn solve1 [path]
  (->> (read-input path)
    (simulate-n-days simulate-day 80)
    count))


(solve1 "sample.txt")
(solve1 "input.txt")


;; Part 2

;; NOTE(alvaro): We need a more efficient representation of the population, since
;; this section won't fit in memory
;;
;; We compress the information into a single map, which contains how many fishes
;; at each state there are

(defn prepare-population [input]
  "Return a map with a key for each"
  (frequencies input))

(defn update-population [m k to-add]
  (let [old-val (m k 0)]))

(defn update-state-map [m old-state cnt]
  (let [new-state (next-state old-state)
        ;; NOTE(alvaro): More than 1 option updates "6"
        old-count (get m new-state 0)
        new-map (assoc m new-state (+ old-count cnt))]
    (if (> new-state old-state)
      ;; There was a reset, so we spawn new fishes
      (assoc new-map 8 cnt)
      new-map)))


(defn simulate-day-2 [population]
  "Return a map with the new counts for each of the different states"
  (reduce-kv update-state-map {} population))


(defn solve2 [path]
  (->> (read-input path)
    prepare-population
    (simulate-n-days simulate-day-2 256)
    vals
    (reduce + 0)))

(solve2 "sample.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
