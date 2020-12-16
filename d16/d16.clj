(ns d16 (:require [clojure.string :as str]
                  [clojure.test :as test]
                  [clojure.edn :as edn]))

; Shared
(defn- parse-rule
  [s]
  (let [[_ name a a' b b'] (re-matches #"([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)" s)
        lower-a (edn/read-string a)
        upper-a (edn/read-string a')
        lower-b (edn/read-string b)
        upper-b (edn/read-string b')]
    [name [(range lower-a (inc upper-a))
           (range lower-b (inc upper-b))]]))

(test/deftest parse-rule-test
  (test/is (=
            ["class foo" [(list 1 2 3) (list 5 6 7)]]
            (parse-rule "class foo: 1-3 or 5-7"))))

(defn parse-input
  [s]
  (let [[rules _ yours _ others] (-> (str/split-lines s)
                                     (#(partition-by (fn [x] (= "" x)) %1)))
        rules-parsed (into {} (map parse-rule rules))
        others-parsed (->> (map #(str/split %1 #",") others)
                           (drop 1)
                           (map (partial map edn/read-string))
                           vec)
        yours-parsed (->> (drop 1 yours)
                          (map #(str/split %1 #","))
                          (map (partial map edn/read-string))
                          first
                          vec)]
    {:rules rules-parsed :others others-parsed :yours yours-parsed}))

; Part 1
(defn get-invalid-values
  [rules nums]
  (let [range (->> (vals rules)
                   (mapcat (partial map set))
                   (apply concat)
                   set)]
   (clojure.set/difference (set nums) range)))

(test/deftest get-invalid-values-test
  (let [rules {:class [(range 1 4) (range 5 8)]
               :row [(range 6 11) (range 33 44)]
               :seat [(range 13 50)]}]
    (test/is (= #{} (get-invalid-values rules [7 3 47])))
    (test/is (= #{51} (get-invalid-values rules [7 3 51])))))

(defn part1
  [input]
  (let [{:keys [rules others]} (parse-input input)
        all-invalid (->> (map (partial get-invalid-values rules) others)
                         (apply concat))]
    (reduce + all-invalid)))

(def example "class something: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

(comment (part1 example))
(comment (part1 (slurp "./input.txt")))

; Part 2
(defn- get-matching-rules
  "Get the keys of all rules that are valid for the given number n"
  [rules n]
  (let [matches? (fn [[name [range-a range-b]]]
                    (let
                      [range (set (concat (set range-a) (set range-b)))]
                      (contains? range n)))]
    (->> (filter matches? rules)
         keys)))

(test/deftest get-matching-rules-test
  (test/is (=
             [:class :seat]
             (get-matching-rules
                {:class [[1 2 3] [5 6]]
                 :row []
                 :seat [[1 2 3 4 5]]}
                5))))

(defn get-slots
  [rules nums]
  (->> (map (partial get-matching-rules rules) nums)
       (map vec)
       vec))

(defn transpose [a] (apply mapv vector a))

(test/deftest transpose-test
  (test/is (= [[1 3] [2 4]] (transpose [[1 2] [3 4]]))))

; Have fun reading the ->> part ^_^
; We first get all matching rules for each value in each ticket Then we
; transpose that result so we get a list of lists of lists. The inner most
; lists are the matching rules of each ticket for the nth column. So first we
; get all matching rules for the first value in each ticket, then all matching
; rules for the second value in each ticket, and so on.
; Next we get the intersection of all matching rules per column. We now know
; all slots that each column could fit into. There has to be one column which
; only fits into a single slot.
; The intersection part above removed one list layer and now we can fold the
; remaining list of lists, after sorting it so the column with only one
; possible slot comes first. It's important to remember the original positions
; though.
; For each iteration, we get the set difference between all slots we have
; already filled, and all possible slots of the column we're looking at. This
; nicely leads to every new column having only one possible slot after
; eliminating the slots we've already assigned.
; Finally, we restore the original order and that's it.
(defn part2
  [input]
  (let [{:keys [rules yours others]} (parse-input input)
        valid? #(empty? (get-invalid-values rules %))
        slot-order (->> (filter valid? others)
                        (map (partial get-slots rules))
                        transpose
                        (map (partial map set))
                        (map (partial apply clojure.set/intersection))
                        (map-indexed (fn [i v] {:position i :values v}))
                        (sort-by (fn [v] (count (:values v))))
                        (reduce
                          (fn [{:keys [seen slots] :as xs} {:keys [position values] :as obj}]
                            (let [x' (clojure.set/difference values seen)
                                  seen' (apply (partial conj seen) x')
                                  slots' (conj slots {:position position :values x'})]
                              {:seen seen' :slots slots'}))
                          {:seen #{} :slots []})
                        :slots
                        (sort-by :position)
                        (map :values)
                        (apply concat))]
    (->> (map vector yours slot-order)
         (filter #(re-find #"^departure" (second %)))
         (map first)
         (reduce *))))

(def example-p2 "departure row: 0-5 or 8-19
departure seat: 0-13 or 16-19
departure class: 0-1 or 4-19

your ticket:
11,12,13

nearby tickets:
9,3,18
1,15,5
14,5,9")

(comment (part2 example-p2))
(comment (part2 (slurp "./input.txt")))
