(ns
  algorithms.sort
  (:gen-class))

(defn- splice [k fseq tseq]
  (concat (concat fseq (list k)) tseq))

(defn- splice-with 
  ([k pred pair]
     (let [pair (split-with pred pair)]
       (let [fst (first pair)
             sd (second pair)]
           (splice k fst sd)))))

(defn- do-insertion-sort
  "insertion-sort algorithm"
  [j pred s]
   (if (> j (count s)) s
     (let [hseq (take j s) tseq (drop (inc j) s) k (nth s j)]
       (let [new-seq (concat (splice-with k (fn [i] (pred k i)) hseq) tseq)]
         (if (empty? tseq) new-seq
           (recur (inc j) pred new-seq))))))

(defn insertion-sort [pred s]
  (do-insertion-sort 1 pred s))

(defn merge-seq [acc pred s1 s2]
  "merge two sorted sequences"
  (if (or (empty? s1) (empty? s2)) ; are either lists empty?
    (if (empty? s1) ; which one is empty?
      (concat acc s2) ; s1 wasn't empty so s2 must be empty
      (concat acc s1)) ; s1 was empty
    (let [h1 (first s1)
          h2 (first s2)]
      (if (pred h1 h2)
        (let [new-acc (cons h1 acc)]
          (merge-seq new-acc pred (drop 1 s1) s2))
        (let [new-acc (cons h2 acc)]
          (merge-seq new-acc pred (drop 1 s2) s1))))))

(defn merge-sort
  "merge-sort algorithm"
  [pred s]
  (let [seq-size (count s)]
    (println "sequence is: " s)
    (println "size of sequence is: " seq-size)
    (if (<= seq-size 1)
      ((println "size was 1 or less returning: " s)
         s)
      ((println "size was 2 or more continuing")
       (let [lst1 (take (int (/ seq-size 2)) s)
            lst2 (drop (int(/ seq-size 2)) s)]
        (println "lst1: " lst1)
        (println "lst2: " lst2)
        ; TODO(jwall): detect the leaf case
        (if (and (<= (count lst1) 1) (<= (count lst2) 1))
          ((println "found the leaf case merging: " lst1 lst2)
            (let [merged (merge-seq () pred lst1 lst2)]
              (println "merged: " merged)
              merged))
          (let [new-lst1 ((println "sorting lst1: " lst1)
                            (merge-sort pred lst1)) ; sort lst1 recursively
                new-lst2 ((println "sorting lst2: " lst2)
                            (merge-sort pred lst2))] ; sort lst2 recursively
            (println "new-lst1: " new-lst1)
            (println "new-lst2: " new-lst2)
            (merge-seq () pred new-lst1 new-lst2)))
         )))))

; benchmarking code
(defn time-insertion-sort [n]
  (time (insertion-sort >= (take n (repeatedly (fn [] (rand-int 100)))))))

(defn time-merge-sort [n]
  (time (merge-sort >= (take n (repeatedly (fn [] (rand-int 100)))))))

; example test code
; (defn c-sort-code [] (compile 'algorithms.sort) (refer 'algorithms.sort))
; (c-sort-code)
; (defn test-merge-sort [n] (c-sort-code) (time-merge-sort n))
; (test-merge-sort 10)

