;; Day 16
(require '[clojure.string :as str])

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
      str/trim))

(def hex->binary
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(defn parse-bin-int [s]
  "Parse a binary integer"
  (Long/parseLong s 2))

(defn binarify [s]
  "Convert a string of uppercase hex values to binary"
  (apply str (map hex->binary s)))


;; NOTE(alvaro): The `consume-` family of functions take the bits (and maybe)
;;  other relevant arguments and return their value and the rest of the bits
;;  that they did not consume
(defn build-packet [version type-id pretty value]
  {:version version
   :type-id type-id
   :pretty pretty
   :value value})


(defn no-info-remaining? [remaining]
  "Returns `true` if there is no relevant information remaining"
  (or (empty? remaining)
      (every? (partial = \0) remaining)))


(defn assert-no-info-remaining [remaining]
  (assert (no-info-remaining? remaining)
    (str "Found unexpected remaining information after processing" remaining)))


;; NOTE(alvaro): Due to the way this is structured, these consume functions
;; are mutually recursive, and to solve it we forward declare the consume-packet
;; and it will be actually defined later on
(declare consume-packet)

(defn consume-literal [bits]
  "Parse a literal value from the head of the bits.
  Returns the parsed literal and the remaining bits"
  (assert (>= (count bits) 5))
  (let [bit-groups (partition 5 bits)
        starts-with-1? #(= (first %) \1)
        one-groups (into [] (take-while starts-with-1? bit-groups))
        last-group (first (drop-while starts-with-1? bit-groups))
        sel-groups (conj one-groups last-group)
        sel-bit-count (apply + (map count sel-groups))
        bin-lit (apply str (flatten (map rest sel-groups)))]
    [(parse-bin-int bin-lit)
     (subs bits sel-bit-count)]))


(defn consume-packet [bits]
  "Consume a packet from the bits"
  (let [version (parse-bin-int (subs bits 0 3))
        type-id (parse-bin-int (subs bits 3 6))
        rest-bits (subs bits 6)]
    ;; Check a packet based on its type
    (if (= type-id 4)
      (let [[literal rest-bits] (consume-literal rest-bits)]
        [(build-packet version type-id "Literal" literal)
         rest-bits])
      (let [[result rest-bits] (consume-operator rest-bits)]
        [(build-packet version type-id "Operator" result)
         rest-bits]))))


(defn consume-packet-with-size [bits]
  "Consume a packet prepended with a 15 bit size"
  (let [size-bits (subs bits 0 15)
        size (parse-bin-int size-bits)
        packet-end (+ 15 size)
        packet-bits (subs bits 15 packet-end)
        rest-bits (subs bits packet-end)]
    ;; We consume packets one after the other until the full size is complete
    ;; We will assert that after parsing everything nothing of value is left
    (loop [packets []
           bits packet-bits]
      (if
        (no-info-remaining? bits)
        ;; We are done processing
        [packets rest-bits]
        (let [[packet bits] (consume-packet bits)]
          (recur
            (conj packets packet)
            bits))))))


(defn consume-n-packets [bits]
  "Consume multiple packets prepended with a 11 bit `n` number"
  (let [n-bits (subs bits 0 11)
        n (parse-bin-int n-bits)
        rest-bits (subs bits 11)]
    ;; Apply the `consume-packet` function `n` times each time with the `rest-bytes`
    (loop [i n
           packets []
           bits rest-bits]
      (if (<= i 0)
        ;; Return the accumulated result
        [packets bits]
        (let [[packet bits] (consume-packet bits)]
          (recur
            (dec i)
            (conj packets packet)
            bits))))))


(defn consume-operator [bits]
  "Consume the an operator packet (and its subpackets) from the bits"
  (let [length-type-id (first bits)
        rest-bits (subs bits 1)]
    (if
      (= length-type-id \0)
      ;; Consume a single packet with expected size
      (consume-packet-with-size rest-bits)
      ;; Consume n packets
      (consume-n-packets rest-bits))))


(defn sum-versions [packet]
  "Sum all the version numbers from a packet and its subpackets"
  (if (= (:pretty packet) "Literal")
    ;; This packet is a leaf, so just return it's version
    (:version packet)
    ;; This packet contains subpackets, so we need to recurse
    (reduce
      +
      (:version packet)
      (map sum-versions (:value packet)))))

;; Test
(defn view-packet [hex]
  (->> hex
       binarify
       consume-packet
       first))

(view-packet "8A004A801A8002F478")
(sum-versions (view-packet "8A004A801A8002F478"))
(sum-versions (view-packet "620080001611562C8802118E34"))
(sum-versions (view-packet "C0015000016115A2E0802F182340"))
(sum-versions (view-packet "A0016C880162017C3686B18A3D4780"))

;; Part 1
(defn solve1 [path]
  (->> path
       read-input
       binarify
       consume-packet
       first
       sum-versions))

(solve1 "sample.txt")
(solve1 "input.txt")

;; Part 2
(declare evaluate)

(defn assert-not-called [& args]
  "This function should not be called"
  (throw (AssertionError. (str "Unexpected function call with args: " args))))

;; Operators
(def operators
  "List of operations associated to each `:type-id` (by index)"
  [+ * min max assert-not-called > < =])

(defn bool->int [bool]
  (if bool 1 0))

(defn operator [op-fn args]
  "Apply an operator to a packet"
  (let [result (apply op-fn args)]
    ;; Make sure we transform the booleans back to numbers
    (if (boolean? result)
      (bool->int result)
      result)))

;; Evaluation function
(defn evaluate [packet]
  "Evaluate a single packet"
  (if (= (:type-id packet) 4)
    ;; This is a literal, return the value directly
    (:value packet)
    ;; This is an operation, apply it to the evaluated values
    (operator (operators (:type-id packet)) (map evaluate (:value packet)))))

;; Tests
(def test-eval (comp evaluate view-packet))
(test-eval "C200B40A82")
(test-eval "04005AC33890")
(test-eval "880086C3E88112")
(test-eval "CE00C43D881120")
(test-eval "D8005AC2A8F0")
(test-eval "F600BC2D8F")
(test-eval "9C005AC2F8F0")
(test-eval "9C0141080250320F1802104A08")


(defn solve2 [path]
  (->> path
       read-input
       binarify
       consume-packet
       first
       evaluate))

(solve2 "sample.txt")
(solve2 "input.txt")

;; Main
(println (str "Solution 1: " (solve1 "input.txt")))
(println (str "Solution 2: " (solve2 "input.txt")))
