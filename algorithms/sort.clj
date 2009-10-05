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

(defn insertion-sort
  "insertion-sort algorithm"
  [j pred s]
   (if (> j (count s)) s
     (let [hseq (take j s) tseq (drop (inc j) s) k (nth s j)]
       (let [new-seq (concat (splice-with k (fn [i] (pred k i)) hseq) tseq)]
         (if (empty? tseq) new-seq
           (recur (inc j) pred new-seq))))))

; benchmarking code
(defn time-insertion-sort [n]
   (time (insertion-sort 1 >= (take n (repeatedly (fn [] (rand-int 100)))))))

