(ns app.fun.randomname
  (:require [clojure.string :as string :refer [join]]))

(def data
  {:name [:adj :noun]
   :adj  [["super" "ultra" "pro" "surprising"]]
   :noun [["node" "point" "port"]]})

(defn pick-random [vec]
  (nth vec (.floor js/Math (rand (count vec)))))

(defn random-name
  ([] (random-name :name 0))
  ([terminal depth]
   (let [read (data terminal)]
     (if (or (nil? read)
             (> depth 10))
       (str terminal)
       (join "-" (for [item read]
                   (cond
                     (keyword? item) (random-name item (+ 1 depth))
                     (vector? item)  (random-name (pick-random item) (+ 1 depth))
                     :else item)))))))
