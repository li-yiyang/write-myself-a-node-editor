(ns app.fun.helpers)

(defn map-map [f kvs]
  (into {} (map (fn [[k v]] [k (f v)]) kvs)))
