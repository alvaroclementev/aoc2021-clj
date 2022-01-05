;; Day 17
(require '[clojure.string :as str])

;; Instead of using the movement equations, let's use a simulation
;; based method

;; NOTE(alvaro): There's a much smarter (and faster) way to solve this
;; without simulating everything, by realizing that:
;;  1. The actual positions that can be filled are discrete: most of the
;;      positions are impossible to achieve so we don't need to check for
;;      them
;;  2. There are symmetries in this problem: x is perfectly symmetric, and
;;      then on the ys we can see that when you throw something up, it will
;;      always take the same path back, and then arrive at 0 with a (dec (- vy))
;;      speed, which you can then use to guess where you will go through
;;  3. The maximum value can be computed a priori by summing the natural
;;      numbers up to `vy` (and `vx` as well, incidentally)

(defn debug [x]
  "Simple debug function to insert in the middle of a thread macro"
  (println x)
  x)

;; Utils
(defn split-lines [text]
  (str/split text #"\n"))

(defn read-input [path]
  "Read the input file"
  (->> path
       slurp
       str/trim
       (#(subs % (count "target area: ")))
       (#(str/split % #", "))
       (map #(str/split (subs % 2) #"\.\."))
       (map (fn [coll]
              (map #(Integer/parseInt %) coll)))
       (map #(into [] %))))

(defn repr-state [[[x y] [vx vy]]]
  (str "State{(" x ", " y "), (" vx ", " vy ")}"))


;; NOTE(alvaro): With this formula we can compute:
;;  1. The final X coordinate
;;  2. The max Y coordinate (if vy is positive)
(defn natural-sum [n]
  "Formula for the natural-sum of first N natural numbers"
  (int (/ (* n (inc n)) 2)))


(defn simulate-step [state]
  "Compute the next position and velocity after one step"
  (let [[[x y] [vx vy]] state
        new-x (+ x vx)
        new-y (+ y vy)
        new-vx (if (> vx 0)
                 (max (dec vx) 0)
                 (min (inc vx) 0))
        new-vy (dec vy)
        new-state [[new-x new-y] [new-vx new-vy]]]
    new-state))

(defn simulate [velocity]
  "Generate an infinite sequence of the values of one simulation
  based on an initial velocity"
  (iterate simulate-step [[0 0] velocity]))


(defn in-range? [v [start end]]
  "Returns `true` if the value is inside the given [start end] range (inclusive)"
  (and (>= v start) (<= v end)))


(defn in-target? [target state]
  "Returns `true` if the given [x y] is inside the target"
  (if (nil? state)
    nil
    (let [[x y] (first state)
          [x-range y-range] target]
      (and
        (in-range? x x-range)
        (in-range? y y-range)))))


(defn toward-target? [target state]
  "Returns `true` if the `state` has not passed yet the target and its moving
  toward it"
  (let [[xs ys] target
        [min-x max-x] xs
        [min-y max-y] ys
        [[x y] [vx vy]] state]
    (and
      ;; Check the xs, which can be stopped
      (cond
        (> vx 0) (<= x max-x)
        (< vx 0) (>= x min-x)
        :else (in-range? x xs))
      ;; Check the ys, which is always decreasing
      (if (< vy 0)
        (>= y min-y)
        true))))


(defn simulation-in-target? [target velocity]
  "Checks if the simulation with `velocity` will pass through a `state` inside the target"
  (in-target?
    target
    (last
      (take-while
        (partial toward-target? target)
        (simulate velocity)))))


(defn max-height [[vx vy]]
  "Return the max height achieved with this velocity"
  (if (<= vy 0)
    0
    (natural-sum vy)))


(defn velocities-in-target [target]
  "Check the velocities that will fall under `target`"
  (let [[[min-x max-x] [min-y max-y]] target]
    (for [vx (range (min min-x 0) (inc (max max-x 0)))
          ;; If you throw something up, it will always arrive at 0 again,
          ;; with a vy of (dec (- vy)). With that in mind we can see that
          ;; an upper range of the vys to test is
          vy (range (min min-y 0) (inc (apply max (map #(Math/abs %) [min-y max-y]))))
          :when (simulation-in-target? target [vx vy])]
      [vx vy])))


;; Test
(simulation-in-target? (read-input "sample.txt") [7 2])
(simulation-in-target? (read-input "sample.txt") [100 2])
(simulation-in-target? (read-input "sample.txt") [17 -4])
(simulation-in-target? (read-input "sample.txt") [-7 3])


;; Part 1
(defn solve1 [path]
  (->> path
       read-input
       velocities-in-target
       (map max-height)
       (apply max)))

(solve1 "sample.txt")
(solve1 "input.txt")

;; Part 2
(defn solve2 [path]
  (->> path
       read-input
       velocities-in-target
       count))

(solve2 "sample.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
