(ns aoc.d13 (:require [clojure.string :as str]
                      [clojure.edn :as edn]))

(def input (-> (slurp "./input.txt")
               (str/split-lines)))

(def example-departure 939)
(def example-times [7,13,"x","x",59,"x",31,19])

; Part 1
(def departure (edn/read-string (nth input 0)))
(def times (-> (nth input 1)
               (#(str/split % #","))
               (#(remove #{"x"} %))
               (#(map edn/read-string %))))

(defn wait-time [dep bus]
  (let [next-dep (* (+ (quot dep bus) 1) bus)]
    (- next-dep dep)))

(defn min-by-value [map]
  (reduce-kv
    (fn
      [acc k v]
      (if (empty? acc) [k v]
        (let [[min-key min-value] acc]
          (if (< v min-value) [k v] [min-key min-value]))))
    []
    map))

(defn wait-times
  [departure times]
  (reduce #(assoc %1 %2 (wait-time departure %2)) {} times))

(defn solve-p1
  [departure times]
  (let [[k v] (min-by-value (wait-times departure times))]
    (* k v)))

(comment (solve-p1 departure times))

; Part 2
(defn align-cycles
  [{idA :id diffA :diff} {idB :id diffB :diff}]
  (let [p (* idA idB)
        valid (first (for [t (iterate #(+ idA %) diffA)
                           :when (= (rem (+ t diffB) idB) 0)] t))]
    {:id p :diff valid}))

(defn solve-p2
  [times]
  (let [xs (map-indexed (fn [i v] {:id v :diff i}) times)
        nums (filter (fn [{v :id}] (number? v)) xs)]
    (reduce align-cycles nums)))

(comment (filter (fn [{v :id}] (number? v)) [{:id "foo"} {:id 5}]))

(def times-p2 (-> (nth input 1)
               (#(str/split % #","))
               (#(map edn/read-string %))))

(comment (solve-p2 [2 7 "x" 5]))
(comment (solve-p2 example-times))
(comment (solve-p2 times-p2))
