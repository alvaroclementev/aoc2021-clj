;; Day 18
(require '[clojure.string :as str])

(defn debug [x]
  "Simple debug function to insert in the middle of a thread macro"
  (println "debug:" x)
  x)

;; Utils
(defn split-lines [text]
  (str/split text #"\n"))

(defn read-input [path]
  "Read the input file"
  (->> path
       slurp
       str/trim
       split-lines
       (map read-string)))

(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (take-until pred (rest s)))))))


;; Tree utils
(defn tree-depth [tree]
  "Get the maximum depth of the tree"
  ;; Depth first search for some component to be exploded
  (loop [max-depth 1
         queue (list [(first tree) 1] [(second tree) 1])]
    (if (empty? queue)
      ;; We are done, return the max depth
      max-depth
      ;; Let's check the next element in the queue
      (let [[el depth] (first queue)
            new-max-depth (if (> depth max-depth)
                            depth
                            max-depth)]
        (if (number? el)
          (recur new-max-depth
                 (rest queue))
          (let [[left right] el
                new-queue (rest queue)
                new-depth (inc depth)]
            (recur new-max-depth
                   (conj
                     (conj new-queue [right new-depth])
                     [left new-depth]))))))))


(defn depth-first-search-condition [pred n]
  "Perform a depth first search looking pairs for which `(pred pair depth)` is `true`
  and return its position (indices) inside the `n` (vector, compatible with `(get-in)`)"
  ;; Depth first search for some component to be exploded
  ;; A candidate in the queue is a pair [number depth]
  (loop [queue (list [(first n) 0 [0]] [(second n) 0 [1]])]
    (if (empty? queue)
      ;; We are done, nothing to be done here
      nil
      ;; Let's check the next element in the queue
      (let [[n depth pos] (first queue)]
        (if (pred n depth)
          ;; Return the position once we are done
          pos
          (if (number? n)
            (recur (rest queue))
            (let [[left right] n
                  new-queue (rest queue)
                  new-depth (inc depth)]
              (recur (conj
                       (conj new-queue [right new-depth (conj pos 1)])
                       [left new-depth (conj pos 0)])))))))))


;; In these direction dependant fucntions we consider 0 = left, 1 = right
(defn tree-extreme-pos
  "Find the position of the extreme moving in `direction`"
  ([direction tree]
   (tree-extreme-pos direction tree []))
  ([direction tree pos]
    (if (number? tree)
      pos
      (recur direction (tree direction) (conj pos direction)))))


;; Find a potential adjacent's common ancestor
(defn adjacent-common-ancestor [direction n pos]
  "Find the position of the common ancestor to `pos` in `n` in a `direction`"
  (let [ancestor-pos (->> pos
                          reverse
                          (drop-while (partial = direction))
                          (drop 1)
                          reverse)]
    (when (seq ancestor-pos)
      (into [] ancestor-pos))))


(defn find-adjacent-pos [direction n pos]
  "Find the position of the number adjacent to `pos` in `n`, moving in `direction`.
  If there are no adjacent positions in that direction, it returns `nil`"
  ;; Look for the first non-direction index in the pos, as that would be where
  ;; the tree adjancent (in direction) is
  (if (every? (partial = direction) pos)
    ;; There are no elements in that direction
    nil
    ;; Find the corresponding element
    (let [ancestor-pos (adjacent-common-ancestor direction n pos)
          ancestor (get-in n ancestor-pos)
          opposite (- 1 direction)
          ;; Move once in the direction, then find the extreme opposite
          dir-rel-pos (tree-extreme-pos opposite (ancestor direction))]
      (into [] (concat ancestor-pos [direction] dir-rel-pos)))))


;; this will be useful when threading operations in
(defn maybe-update-in [coll pos f]
  "Update the collection if `pos` is not `nil`"
  (if (seq pos)
    (update-in coll pos f)
    coll))


;; Snailfish operations
(defn explode-available [n]
  "Check if `n` has some component that can be exploded"
  (depth-first-search-condition (fn [n depth]
                                  (and (vector? n)
                                       (>= depth 3))) n))


(defn split-available [n]
  "Check if `n` has some component that can be split"
  (depth-first-search-condition (fn [n _]
                                  (and (number? n)
                                       (>= n 10))) n))




(defn snailfish-explode [n pos]
  "Explode the pair at position `pos` in `n`"
  ;; Find the next number to the left and to the right
  (let [pair (get-in n pos)
        [left right] pair
        left-pos (find-adjacent-pos 0 n pos)
        right-pos (find-adjacent-pos 1 n pos)]
    (-> n
      ;; Update the left value
      (maybe-update-in left-pos (partial + left))
      ;; Update the right value
      (maybe-update-in right-pos (partial + right))
      ;; Change the current pair into a 0
      (assoc-in pos 0))))


(defn snailfish-split [n pos]
  "Split regular number at position `pos` in `n`"
  (let [reg-n (get-in n pos)
        half-reg-n (/ reg-n 2)
        new-left (int (Math/floor half-reg-n))
        new-right (int (Math/ceil half-reg-n))]
    (assoc-in n pos [new-left new-right])))


(defn snailfish-reduce [n]
  "Apply the reduction process to the snailfish number `n`"
  (loop [n n]
    (let [explode-pos (explode-available n)]
      (if explode-pos
        (recur (snailfish-explode n explode-pos))
        (let [split-pos (split-available n)]
          (if split-pos
            (recur (snailfish-split n split-pos))
            ;; No more reducing operations are available, `n` is in its reduced
            ;; form
            n))))))

(defn snailfish-add [n1 n2]
  "Snailfish addition"
  (snailfish-reduce [n1 n2]))


(defn snailfish-magnitude [n]
  "Compute the magnitude of the snailfish number `n`"
  (if (number? n)
    n
    ;; It's a pair
    (+ (* 3 (snailfish-magnitude (first n)))
       (* 2 (snailfish-magnitude (second n))))))


;; Test
(read-input "sample.txt")
(read-input "input.txt")

(defn test-explode [s]
  (let [n (read-string s)
        explode-pos (explode-available n)]
    (if explode-pos
      (snailfish-explode n explode-pos)
      "No explode available")))

(defn test-split [s]
  (let [n (read-string s)
        split-pos (split-available n)]
    (if split-pos
      (snailfish-split n split-pos)
      "No split available")))

(defn test-reduce [s]
  (snailfish-reduce (read-string s)))

(defn test-add [s1 s2]
  (snailfish-add (read-string s1) (read-string s2)))

(defn test-magnitude [s]
  (snailfish-magnitude (read-string s)))

;; Explodes
(def TEST-STRING "[[[[[9,8],1],2],3],4]")
(def TEST-STRING "[7,[6,[5,[4,[3,2]]]]]")
(def TEST-STRING "[[6,[5,[4,[3,2]]]],1]")
(test-explode TEST-STRING)
(test-reduce TEST-STRING)

;; Explodes
(def TEST-STRING "[[[[0,7],4],[15,[0,13]]],[1,1]]")

(test-split TEST-STRING)

;; Full reduce
(test-reduce "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")

;; Full reduce
(test-add "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]")

;; Magnitude
(test-magnitude "[[1,2],[[3,4],5]]")
(test-magnitude "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")

;; Part 1
(defn solve1 [path]
  (->> path
       read-input
       (reduce snailfish-add)
       snailfish-magnitude))

(solve1 "sample.txt")
(solve1 "sample-large.txt")
(solve1 "input.txt")

;; Part 2
(defn all-pairs-no-dups [nums]
  "Return a sequence of all pairs of snailish numbers, except for each with
  itself"
  (for [i (range (count nums))
        j (range (count nums))
        :when (not= i j)
        :let [n1 (nth nums i)
              n2 (nth nums j)]]
    [n1 n2]))

(defn solve2 [path]
  (->> path
       read-input
       all-pairs-no-dups
       (map #(snailfish-add (first %) (second %)))
       (map snailfish-magnitude)
       (apply max)))

(solve2 "sample.txt")
(solve2 "sample-large.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
