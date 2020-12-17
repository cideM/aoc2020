(ns d17 (:require [clojure.string :as str]
                  [clojure.test :as test]
                  [clojure.test :as test]
                  [clojure.edn :as edn]
                  [clojure.math.combinatorics :as combo]))

(comment (def example-p1
           ".#.
            ..#
            ###"))

(comment (def example-map (parse-input example-p1)))

; Shared
(defn get-neighbours
  [m {:keys [x y z]}]
  (let [vectors (remove #{[0 0 0]} (combo/selections [-1 1 0] 3))]
   (map
     (fn [[dx dy dz]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})
     vectors)))

(defn new-state-for-pos
  [m pos]
  (let [active (->> (get-neighbours m pos)
                    (map (partial get m))
                    (filter (partial = \#))
                    count)
        current-active (= \# (m pos))]
    (cond
      (and current-active (#{2 3} active)) \#
      (and (not current-active) (= active 3)) \#
      :else \.)))

(defn grow
  [m]
  (->> (keys m)
       (mapcat (partial get-neighbours m))
       (map #(hash-map % \.))
       (apply merge)
       (#(merge % m))))

(defn new-state-all
  [m]
  (let [m' (grow m)
        reducer #(assoc %1 %2 (new-state-for-pos m' %2))]
    (->> (keys m')
         (reduce reducer {}))))

(defn evolve
  [initial max]
  (loop [n 0
         m initial]
    (cond
      (= n max) m
      :else (recur (inc n) (new-state-all m)))))

(defn get-active-count
  [m]
  (->> (vals m)
       (filter (partial = \#))
       count))

(defn parse-input
  [s]
  (->> (str/split-lines s)
       (map-indexed
         (fn [y row] (map-indexed #(hash-map {:x %1 :y y :z 0} %2) (vec row))))
       flatten
       (apply merge)))

; Solve Part 1
(comment (-> (slurp "./input.txt")
             parse-input
             (evolve 6)
             get-active-count))

; Solve Part 2
(defn get-neighbours-p2
  [m {:keys [x y z w] :as pos}]
  (let [vectors (remove #(= % (list 0 0 0 0)) (combo/selections [-1 1 0] 4))]
   (map
     (fn
       [[dx dy dz dw]]
       {:x (+ x dx) :y (+ y dy) :z (+ z dz) :w (+ w dw)})
     vectors)))

(defn parse-input-p2
  [s]
  (->> (str/split-lines s)
       (map-indexed
         (fn [y row] (map-indexed #(hash-map {:x %1 :y y :z 0 :w 0} %2) (vec row))))
       flatten
       (apply merge)))

(comment (with-redefs [get-neighbours get-neighbours-p2]
           (-> (slurp "./input.txt")
               parse-input-p2
               (evolve 6)
               get-active-count)))
