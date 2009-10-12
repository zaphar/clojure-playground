(ns
  list.util
  (:gen-class))

(defn foldl
  ([acc pred s]
    (if (= s ())
      acc
      (let [hd (first s)
            nxt (pred acc hd)
            tl (drop 1 s)]
        (recur nxt pred tl))))
  ([acc pred] acc))
