(ns
  com.marzhillstudios.list.util
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

(defn foldr
  ([acc pred s]
   (if (= s ())
     acc
     (let [hd (first s)
           tl (drop 1 s)]
       (pred hd (foldr acc pred tl))))))
