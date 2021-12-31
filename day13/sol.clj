; Day 13
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
      prepare-instructions
      (into [])))


(defn process-instruction-line [inst-line]
  (let [[axis v] (str/split
                   (str/replace inst-line "fold along " "")
                   #"=")]
    [(keyword axis) (Integer/parseInt v)]))

(defn prepare-instructions [lines]
  "Read the lines and process the instructions"
  (let [[dot-lines fold-lines]
        (map #(remove empty? %) (partition-by #(str/includes? % ",") lines))
        dots (map
               (fn [dot-line]
                 (apply vector (map #(Integer/parseInt %) (str/split dot-line #",")))) dot-lines)
        fold-insts (map process-instruction-line fold-lines)]
    [dots fold-insts]))


;; FIXME(alvaro): For now we are assuming that we are never folding more
;; than half the page (that is, the points with lower coordinate in the)
;; fold don't change coordinates after folding
(defn fold-dot [[x y] [axis v]]
  "Return the position of the dot at [x y] after being folded along `axis` at
  value `v`"
  (assert (or (= axis :x) (= axis :y)))
  (if
    (= axis :x)
    ;; Fold along X axis
    (if (= x v)
      nil
      [(if
         (< x v)
         x
         (rem (- v (rem x v)) v))
       y])
    ;; Else fold along Y axis
    (if (= y v)
      nil
      [x
       (if
         (< y v)
         y
         (rem (- v (rem y v)) v))])))


(defn fold [dots inst]
  "Apply a fold to a set of dots given an instruction. This can generate
  duplicate dots"
  (keep #(fold-dot % inst) dots))


(defn debug [x]
  "Simple debug function to insert in the middle of a thread macro"
  (println x)
  x)

(defn render-dots [dots]
  "Visualize the position of the dots"
  (let [dot-set (into #{} dots)
        cols (inc (apply max (flatten (map first dots))))
        rows (inc (apply max (flatten (map second dots))))
        characters (for [y (range rows)
                         x (range cols)]
                     (if (contains? dot-set [x y])
                       \#
                       \.))]
    (println (str/join
               "\n"
               (map
                 #(apply str %)
                 (partition cols characters))))))


;; Test
(process-instruction-line "fold along y=7")

(prepare-instructions
  (split-lines (slurp "sample.txt")))

(fold-dot [6 0] [:x 5])
(fold-dot [5 0] [:x 5])
(fold-dot [4 0] [:x 5])
(fold-dot [0 13] [:y 7])
(fold-dot [0 14] [:y 7])


(render-dots (first (read-input "sample.txt")))

(let [[dots insts] (read-input "sample.txt")]
  (render-dots (fold dots (first insts))))


(defn apply-folds [dots folds]
  (reduce fold dots folds))

;; Part 1
(defn solve1 [path]
  (let [[dots insts] (read-input path)]
    (count (into #{} (apply-folds dots (list (first insts)))))))


(solve1 "sample.txt")
(solve1 "input.txt")


;; Part 2
(defn solve2 [path]
  (render-dots (apply apply-folds (read-input path))))

(solve2 "sample.txt")
(solve2 "input.txt")
;; The code was "BLHFJPF"

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
