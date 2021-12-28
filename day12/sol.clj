; Day 11
(require '[clojure.string :as str])

;; Utils
(defn split-lines [text]
  (str/split text #"\n"))

(defn process-line [line]
  (str/split (str/trim line) #"-"))

(defn read-input [path]
  "Read the input file"
  (->> path
      slurp
      str/trim
      split-lines
      (map process-line)
      (into [])))

(defn debug [x]
  "Simple debug function to insert in the middle of a thread macro"
  (println x)
  x)

(defn graph-node [graph [from to]]
  "Add the `from`->`to` edge to the graph and `to`->`from`"
  ;; to->from
  (assoc
    ;; from->to
    (assoc
      graph
      from
      (conj (get graph from #{}) to))
    to
    (conj (get graph to #{}) from)))

(defn build-graph [instructions]
  "Build a graph from a list of instructions"
  (reduce graph-node {} instructions))

(defn valid-path? [path]
  "Check that a small cave is not visited more than once"
  (->> path
       ;; Get the ones that correspond to all lowercase
       (filter (fn [step] 
                 (every? #(Character/isLowerCase %) step)))
       ;; Get the counts for each of those
       frequencies
       ;; Get the values of the counts
       vals
       ;; false if at least one element is higher
       (every? #(< % 2))))


(defn has-cycles? [path]
  "Check if the directed graph formed by `path` contains a cycle, that is, the same edge is taken more than once in the same direction"
  (let [edges (map vector path (rest path))]
    (not= (count edges)
          (count (into #{} edges)))))

(defn path-candidate? [path pred to]
  "Check if a path would be a valid candidate"
  ;; We want to avoid going back and forth
  ;; We want to avoid circles!
  (and
    ;; It's not finished (handled outside)
    (not= (last path) to)
    ;; It's not going back to the start
    (not= (last path) (first path))
    ;; NOTE(alvaro): This apparently is not an issue
    ;; We are not circling around
    ; (not (has-cycles? path))
    ;; Satisfies the predicate
    (pred path)))

(defn find-paths [graph from to pred]
  "Explore all the possible paths in the graph that satisfy the `pred`"
  (loop [candidates (into [] (map vector (repeat from) (graph from)))
         paths []
         iterations 0]
    ;; Avoid infinite recursion
    (assert (< iterations 500000))
    (if (empty? candidates)
      paths
      ;; Check the next candidate
      (let [path (first candidates)
            node-set (graph (last path))
            potential-candidates (map #(conj path %) node-set)
            new-paths (filter #(= (last %) to) potential-candidates)
            new-candidates (filter #(path-candidate? % pred to) potential-candidates)]
        (recur
          (apply conj (rest candidates) new-candidates)
          (apply conj paths new-paths)
          (inc iterations))))))


(read-input "small.txt")
(valid-path? ["start" "A" "start"])
(valid-path? ["start" "dc" "HN" "kj" "HN" "dc" "end"])
(valid-path? ["start" "HN" "kj" "HN" "dc" "end"])

;; Test
(find-paths
  (build-graph
    (read-input "small.txt"))
  "start"
  "end"
  valid-path?)

(count
  (find-paths
    (build-graph
      (read-input "sample.txt"))
    "start"
    "end"
    valid-path?))

(count
  (find-paths
    (build-graph
      (read-input "large-sample.txt"))
    "start"
    "end"
    valid-path?))

;; Part 1
(defn solve1 [path]
  (count 
    (find-paths
      (build-graph 
        (read-input path))
      "start"
      "end"
      valid-path?)))

(solve1 "small.txt")
(solve1 "sample.txt")
(solve1 "large-sample.txt")
(solve1 "input.txt")


;; Part 2
(defn small-cave-counts [path]
  "Get how many times a small cave appears in the path"
  (->> path
       (filter (fn [step] 
                 (every? #(Character/isLowerCase %) step)))
       (remove #{"start" "end"})
       ;; Get the counts for each of those
       frequencies
       vals))

(defn valid-path2? [path]
  "Check that at most 1 small cave is visited more than once"
  (let [small-counts (small-cave-counts path)
        too-big (filter #(> % 2) small-counts)
        just-enough (filter #(= % 2) small-counts)]
  (and 
    (empty? too-big)
    (< (count just-enough) 2))))

;; Should be valid
(small-cave-counts ["start" "A" "c" "A" "b" "A" "b" "A" "end"])
(valid-path2? ["start" "A" "c" "A" "b" "A" "b" "A" "end"])
;; Should be invalid
(small-cave-counts ["start" "A" "c" "A" "b" "A" "b" "A" "b" "A" "end"])
(valid-path2? ["start" "A" "c" "A" "b" "A" "b" "A" "b" "A" "end"])

(defn solve2 [path]
  (count 
    (find-paths
      (build-graph 
        (read-input path))
      "start"
      "end"
      valid-path2?)))

(solve2 "small.txt")
(solve2 "sample.txt")
(solve2 "large-sample.txt")
(solve2 "input.txt")

;; NOTE(alvaro): This is extremely inefficient. This takes ~1min in my desktop. There should be a better way

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
