(ns d15 (:require [clojure.string :as str]
                  [clojure.edn :as edn]))

(def input (-> (slurp "./input.txt")
               (str/split #",")
               (#(map str/trim %1))
               (#(map edn/read-string %1))
               vec))

(defn solve
  [init until]
  (loop [turn (inc (count init))
         current (last init)
         m (reduce-kv #(assoc %1 %3 [(inc %2)]) {} init)]
    (let [[x y] (m current)]
      (cond
       (= turn (inc until)) current
       (not y) (recur (inc turn)
                      0
                      (assoc m 0 [(last (m 0)) turn]))
       :else (let [new-num (- y x)
                   [x' y'] (m new-num)
                   new-value (cond
                               (and x' y') [y' turn]
                               x' [x' turn]
                               :else [turn])]
                 (recur (inc turn)
                        new-num
                        (assoc m new-num new-value)))))))

(comment (solve input 2020))
(comment (solve input 30000000))
