;; Day 3
(require '[clojure.string :as str])

;; Utils
(defn read-input [path]
  "Read the input file"
  (->> path
      slurp
      (#(str/split % #"\n"))))

(read-input "input-test.txt")

;; Part 1

(defn inc-nil [val]
  "Increment with support for `nil` (treat as 0)"
  (if val (inc val) 1))

;; TODO(alvaro): This could be solved more easily with `frequencies` function
(defn count-bits [counters bits]
  "Maintain a list of counters of the frequency of 0/1 in each position in `bits`"
  (let [actual-counters (if counters
                          counters
                          (initial-counters (count bits)))]
    (map #(update %1 %2 inc-nil) actual-counters bits)))

(defn initial-counters [n]
  "Setup the inital counters"
  (into [] (repeat n {\0 0 \1 0})))

(defn bit-frequencies [words]
  "Compute the frequencies of the values of each bit in a list of bit words"
  (reduce count-bits nil words))

(defn most-common [m]
  "Get the key with the maximum value in `m`"
  (key (apply max-key val m)))

(defn least-common [m]
  "Get the key with the minimum value in `m`"
  (key (apply min-key val m)))

(defn bit-list-to-decimal [bit-list]
  "Parse a list of bits into its decimal form"
  (Long/parseLong (apply str bit-list) 2))

(most-common (frequencies "00011"))
(least-common (frequencies "00011"))

(apply min-key val (frequencies "0011"))

(count-bits nil "00011")
(bit-frequencies ["0011" "1100" "1100"])

(defn solve1 [path]
  (let [words (read-input path)
        freqs (bit-frequencies words)
        gamma (bit-list-to-decimal (map most-common freqs))
        eps (bit-list-to-decimal (map least-common freqs))]
    (* gamma eps)))

(solve1 "input-test.txt")
(solve1 "input.txt")

;; Part 2
(defn most-common-bit [bits]
  "Find the most common element of the collection. In case of tie returns 1"
  (let [freqs (frequencies bits)
        freq-0 (freqs \0)
        freq-1 (freqs \1)]
    (if
      (> freq-0 freq-1) \0 \1)))

(defn least-common-bit [bits]
  "Find the least common element of the collection. In case of tie returns 0"
  (let [most-common (most-common-bit bits)]
    (if (= most-common \0) \1 \0)))

(most-common-bit "11001")
(least-common-bit "11001")
(most-common-bit "11001")
(least-common-bit "11001")
(most-common-bit "1100")
(least-common-bit "1100")

(defn rating-bits [bits bit-criteria]
  (loop [n 0
         bits bits]
    (if (or (= (count bits) 1) (>= n (count (first bits))))
      ;; If we bailout early, this could be the wrong result
      (first bits)
      (let [target-bit (bit-criteria (map #(nth % n) bits))
            _ (println "target bit is " target-bit)]
        (recur
          (inc n)
          (into [] (filter #(= (nth % n) target-bit) bits)))))))

(defn solve2 [path]
  (let [bits (read-input path)]
    (*
      (bit-list-to-decimal (rating-bits bits most-common-bit))
      (bit-list-to-decimal (rating-bits bits least-common-bit)))))

(solve2 "input-test.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
