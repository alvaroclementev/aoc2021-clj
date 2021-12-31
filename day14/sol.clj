; Day 14
(require '[clojure.string :as str])

(defn debug [x]
  "Simple debug function to insert in the middle of a thread macro"
  (println x)
  x)

;; Utils
(defn split-lines [text]
  (str/split text #"\n"))

;; FIXME(alvaro): We are assuming that there's only 1 rule that apply to
;;  the same pair
(defn prepare-instructions [lines]
  "Read the lines and process the instructions.
  The first line is the `template`, then an empty line and then there are
  any number of instructions"
  (let [template (first lines)
        inst-lines (rest (rest lines))
        inst-pairs (map #(str/split % #" -> ") inst-lines)
        instructions (into {} inst-pairs)]
    ;; Make sure we don't find multiple rules for the same pair
    (assert (= (count inst-pairs) (count instructions)))
    [template instructions]))

(defn read-input [path]
  "Read the input file"
  (->> path
      slurp
      str/trim
      split-lines
      prepare-instructions))


(defn polymer-step [template instructions]
  "Insert an element into the `template` based on the instruction `instructions`"
  (apply
    str
    (conj
      (reduce
        (fn [polymer [c next-c]]
          (let [inst-c (instructions (str c next-c))]
            (if inst-c
              ;; Instert the instruction character after the c
              (conj (conj polymer c) inst-c)
              ;; No instruction to apply, so just insert the c
              (conj polymer c))))
        []
        (map list template (rest template)))
      ;; Add the last element of the template which won't be added by the reduce
      (last template))))


(defn apply-n-polymer-steps [template instructions n f]
  "Apply the polymer step `n` times"
  (last (take (inc n) (iterate #(f % instructions) template))))


(defn polymer-score [polymer]
  "Compute the final score of a polymer by taking the frequency of the most and least components"
  (let [freqs (sort (vals (frequencies polymer)))]
    (- (last freqs) (first freqs))))

;; Test
(read-input "sample.txt")

(let [[template instructions] (read-input "sample.txt")]
  (assert (= "NBCCNBBBCBHCB" (apply-n-polymer-steps template instructions 2 polymer-step))))

(let [[template instructions] (read-input "sample.txt")]
  (assert (= "NBCCNBBBCBHCB" (apply-n-polymer-steps template instructions 2 polymer-step))))

(let [[template instructions] (read-input "sample.txt")]
  (assert (= "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB" (apply-n-polymer-steps template instructions 4 polymer-step))))


;; Part 1
(defn solve1 [path]
  (let [[template instructions] (read-input path)]
    (polymer-score (apply-n-polymer-steps template instructions 10 polymer-step))))

(solve1 "sample.txt")
(solve1 "input.txt")

;; Part 2

;; Since we are going to have huge steps, we cannot keep the full polymer
;; in memory.
;; Instead, since the strict order is not relevant, and we only care about
;; To know which characters to insert next we just need to know which
;; pairs of characters exist (and how many of each)
;; Then, to get the final counts, we need to keep a running count of each
;; character, since we lose the concrete character information when we
;; compress it into pairs


;; A polymer will be represented by a map with 2 keys:
;;  `:characters` : a character frequency map
;;  `:pairs` : a character pair frequency map

(defn parse-polymer [template]
  "Build the polymer map from a template"
  {:characters (frequencies template)
   :pairs (frequencies (map #(str %1 %2) template (rest template)))})


(defn smart-sum [& args]
  "+ but it ignores the nil values"
  (apply + (keep identity args)))

(defn compute-changes [polymer instructions]
  "Compute the changes that will need to be applied to each of the maps
  based on the old polymer and the instructions.
  This returns a `changeset`, which is a map with the same keys and as values
  an integer corresponding to the diff, which can then be summed"
  (reduce
    (fn [changeset [pair new-c]]
      (if-not (contains? (:pairs polymer) pair)
        ;; No changes introduced
        changeset
        ;; Compute the new changes
        (let [new-pair1 (str (first pair) new-c)
              new-pair2 (str new-c (second pair))
              pair-count (get-in polymer [:pairs pair])]
          ;; Update the changeset with the new changes introduced by this instruction
          (-> changeset
            (update-in [:characters (first new-c)] smart-sum pair-count)
            ;; We broke the `pair` introducing the character in the middle
            (update-in [:pairs pair] smart-sum (- pair-count))
            (update-in [:pairs new-pair1] smart-sum pair-count)
            (update-in [:pairs new-pair2] smart-sum pair-count)))))
    {:characters {}
     :pairs {}}
    instructions))

(defn apply-map-changes [m changeset]
  "Apply all the "
  (into {}
        (filter #(> (second %) 0)
                (reduce
                  (fn [acc [k v]]
                    (assoc acc k (+ (get m k 0) v)))
                  m
                  changeset))))

(defn apply-changes [polymer changeset]
  "Apply the changes in the changeset into the polymer"
  ;; TODO(alvaro): Remember to remove the keys that are left with 0 as count
  {:characters (apply-map-changes (:characters polymer) (:characters changeset))
   :pairs (apply-map-changes (:pairs polymer) (:pairs changeset))})


(defn polymer-step-map [polymer instructions]
  "Perform an update step on the polymer.
  Since the operations must be performed independently of each other, but
  two operations may affect the same pair, we need to split the work in
  a diff + apply fashion.
  The update step will be run in three phases:
    Compute the operations that we want to apply
    Apply the changes onto the old map
  "
  ;; Apply the changes onto the old polymer
  (apply-changes polymer (compute-changes polymer instructions)))

(defn polymer-score2 [polymer]
  "Compute the final score of a polymer by taking the frequency of the most and least components"
  (let [freqs (sort (vals (:characters polymer)))]
    (- (last freqs) (first freqs))))


;; Tests
(let [[polymer instructions] (read-input "sample.txt")]
  (=
    (polymer-step-map (parse-polymer polymer) instructions)
    (parse-polymer "NCNBCHB")))

(let [[polymer instructions] (read-input "sample.txt")]
  (=
    (polymer-step-map
      (polymer-step-map (parse-polymer polymer) instructions)
      instructions)
    (parse-polymer "NBCCNBBBCBHCB")))

;; Solution
(defn solve2 [path]
  (let [[template instructions] (read-input path)]
    (polymer-score2 (apply-n-polymer-steps
                      (parse-polymer template)
                      instructions
                      40
                      polymer-step-map))))


(solve2 "sample.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
