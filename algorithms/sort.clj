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

; insertion sort sorts in-place and has an 0(n^2) running time
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

(defn- do-merge-seq [acc pred s1 s2]
  (if (or (empty? s1) (empty? s2)) ; are either lists empty?
    (if (empty? s1) ; which one is empty?
      (concat acc s2) ; s1 wasn't empty so s2 must be empty
      (concat acc s1)) ; s1 was empty
    (let [h1 (first s1)
          h2 (first s2)]
      (if (pred h1 h2)
        (let [new-acc (concat acc (list h1))]
          (recur new-acc pred (drop 1 s1) s2))
        (let [new-acc (concat acc (list h2))]
          (recur new-acc pred (drop 1 s2) s1))))))

(defn merge-seq [pred s1 s2]
  "merge two sorted sequences"
  (do-merge-seq '() pred s1 s2))

; merge sort does not sort in place and has an 0(n lg n) running time
(defn merge-sort [pred s]
  "merge-sort algorithm"
  (let [seq-size (count s)]
    (if (<= seq-size 1)s
      (let [lst1 (take (int (/ seq-size 2)) s)
            lst2 (drop (int(/ seq-size 2)) s)]
        (if (and (<= (count lst1) 1) (<= (count lst2) 1))
          (merge-seq pred lst1 lst2)
          (let [new-lst1 (merge-sort pred lst1) ; sort lst1 recursively
                new-lst2 (merge-sort pred lst2)] ; sort lst2 recursively
            (merge-seq pred new-lst1 new-lst2)))))))

(defn- b-heap-parent [i] (int (/ i 2)))
(defn- b-heap-left [i] (* i 2))
(defn- b-heap-right [i] (+ (* i 2) 1))
;(defn- heap-extract-max [] )
;(defn- heap-insert [] )

(defn exchange [i j s]
  (let [hd (take (- i 1) s)
        ith (nth s (- i 1))
        mid (subvec (vec s) i (- j 1))
        jth (nth s (- j 1))
        tl (drop j s)]
    (println "hd: " hd)
    (println "ith: " ith)
    (println "mid: " mid)
    (println "jth: " jth)
    (println "tl: " tl)
    (splice ith (splice jth hd mid) tl)))

(defn- heapify [pred i s] (let [l (nth (b-heap-left i) s)
                                r (nth (b-heap-right i) s)
                                top (nth s 0)
                                ith (nth s i)
                                heap-size (count s)]
                            (def largest 0)
                            (if (and (pred l heap-size) (pred l top))
                              (ref-set largest l)
                              ((ref-set largest ith)))
                            (if (and () ())
                              (ref-set largest r)
                              (when (not (= largest i))
                                (let [s1 (exchange i largest s)]
                                  ())))))

(defn heap-sort [] )

; benchmarking code
(defn- benchmark-sort [srt n msg]
  (println msg "for " n)
  (time (srt >= (take n (repeatedly (fn [] (rand-int 100)))))))

(defn time-insertion-sort [n]
  (benchmark-sort insertion-sort n "Timing insertion sort" ))

(defn time-merge-sort [n]
  (benchmark-sort merge-sort n "Timing merge sort" ))

(defn compare-sorts [n]
  (time-merge-sort n)
  (time-insertion-sort n)
  ())

; example test code
; (defn c-sort-code [] (compile 'algorithms.sort) (refer 'algorithms.sort))
; (c-sort-code)
; (defn test-merge-sort [n] (c-sort-code) (time-merge-sort n))
; (test-merge-sort 10)

