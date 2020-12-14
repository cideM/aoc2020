(ns d14 (:require [clojure.string :as str]
                  [clojure.edn :as edn]
                  [clojure.pprint :refer (cl-format)]
                  [clojure.test :as test]
                  [clojure.math.combinatorics :as combo]))

; Shared functions useful for both parts
(defn num-to-binary
  "Format a number as a 36 bit binary string. Can handle numbers as strings."
  [n]
  (cl-format nil "~36,'0b" (if (number? n) n (edn/read-string n))))

(defn match-set-mask
  "Try to match a line that sets the current mask."
  [s]
  (when-let [matches (re-find #"mask = (\w+)" s)]
    (nth matches 1)))

(test/deftest match-set-mask-test
  (test/is (= "1234" (match-set-mask "mask = 1234")))
  (test/is (= "XX1234" (match-set-mask "mask = XX1234"))))

(defn match-set-value
  "Try to match a line that writes to a register."
  [s]
  (when-let [matches (re-find #"mem\[(\d+)\] = (\d+)" s)]
    {:register (nth matches 1) :value (edn/read-string (nth matches 2))}))

(test/deftest match-set-value-test
  (test/is (= {:register "1" :value 200} (match-set-value "mem[1] = 200"))))

(defn solve
  "Takes a reducer function f to solve either part 1 or part 2"
  [f lines]
  (->> (reduce f {} lines)
      (:registers)
      (vals)
      (reduce +)))

; Part 1 functions
(defn apply-mask
  "Generate a new decimal number from comparing the mask and the given
  number (transformed into a binary string internally).
  
  A new binary number is created based on the part 1 rules. That number
  is then turned into a decimal number and returned."
  [mask num]
  (let [binary (num-to-binary num)
         masked (map (fn [n m] (if (= m \X) n m)) (seq binary) (seq mask))
        joined (str/join masked)]
    (Long/parseLong (str/join masked) 2)))

(test/deftest apply-mask-test
  (test/is (= 73 (apply-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 11)))
  (test/is (= 101 (apply-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 101)))
  (test/is (= 64 (apply-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 0))))

(defn apply-line
  "Parse a single line of input according to the part 1 rules and
  update the state of the program.
  
  Meant to be used as a reducer function for solve."
  [{mask :mask regs :registers :as acc} line]
  (if-let
    [new-mask (match-set-mask line)]
    {:mask new-mask :registers regs}
    (let [{:keys [register value]} (match-set-value line)
          masked (apply-mask mask value)]
      {:mask mask :registers (assoc regs register masked)})))

(test/deftest apply-line-test
  (test/is (= {:mask "123" :registers nil} (apply-line {} "mask = 123")))
  (test/is (=
             {:mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" :registers {"1" 73}}
             (apply-line {:mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"} "mem[1] = 11"))))

; Part 2 functions
(defn- replace-x
  "Replace every nth X with the nth character from replacements.
  The count of replacements should match the occurence of X in string."
  [replacements string]
  (reduce (fn [s r] (str/replace-first s #"X" (str r))) string (seq replacements)))

(defn gen-nums-from-mask
  "Generate all unique numbers from the given mask as binary strings."
  [m]
  (let [slots (re-seq #"X" m)
        perms (combo/selections [1 0] (count slots))
        applied (map (fn [v] (replace-x v m)) perms)]
    (set applied)))

(test/deftest gen-nums-from-mask-test
  (test/is (= (gen-nums-from-mask "000000000000000000000000000000X1101X")
              #{"000000000000000000000000000000011010"
                "000000000000000000000000000000011011"
                "000000000000000000000000000000111010"
                "000000000000000000000000000000111011"})))

(defn apply-mask-to-num
  "Returns a new mask from applying the given mask to the given number
  according to the part 2 rules. This is the part 2 equivalent to
  apply-mask"
  [addr mask]
  (str/join (map
              (fn [a b]
                (cond
                 (= b \0) a
                 (= b \1) \1
                 (= b \X) \X))
              (seq addr)
              (seq mask))))

(test/deftest apply-mask-to-num-test
  (test/is (= (apply-mask-to-num
                "000000000000000000000000000000101010"
                "000000000000000000000000000000X1001X")
              "000000000000000000000000000000X1101X")))

(defn apply-line-p2
  "Parse a single line of input according to the part 2 rules and
  update the state of the program.
  
  Meant to be used as a reducer function for solve."
  [{mask :mask regs :registers :as acc} line]
  (if-let
    [new-mask (match-set-mask line)]
    {:mask new-mask :registers regs}
    (let [{:keys [register value]} (match-set-value line)
          reg-bin (num-to-binary register)
          blueprint (apply-mask-to-num reg-bin mask)
          regs-to-update (map #(Long/parseLong %1 2) (gen-nums-from-mask blueprint))
          written (reduce #(assoc %1 %2 value) regs regs-to-update)]
      {:mask mask :registers written})))

(test/deftest apply-line-p2-test
  (test/is (= {:mask "123" :registers nil} (apply-line-p2 {} "mask = 123")))
  (test/is (=
             {:mask "000000000000000000000000000000X1001X",})) :registers {26 100, 58 100, 59 100, 27 100}
             (apply-line-p2 {:mask "000000000000000000000000000000X1001X"} "mem[42] = 100"))

; Read input
(def input (-> (slurp "./input.txt")
               (str/split-lines)))

; Solve part 1
(def test-program ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                   "mem[8] = 11"
                   "mem[7] = 101"
                   "mem[8] = 0"])

(comment (solve apply-line test-program))
(comment (solve apply-line input))

; Solve part 2
(def test-program-p2 ["mask = 000000000000000000000000000000X1001X"
                      "mem[42] = 100"
                      "mask = 00000000000000000000000000000000X0XX"
                      "mem[26] = 1"])

(comment (solve apply-line-p2 test-program-p2))
(comment (solve apply-line-p2 input))
