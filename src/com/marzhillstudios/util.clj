(ns
  com.marzhillstudios.util
  (:gen-class)
  (:use clojure.contrib.pprint))

(defn print-class-path []
  (pprint (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))))

(defn add-type
  ([nmsp t target]
   (add-type (keyword nmsp t) target))
  ([t target]
   (with-meta target {:type t})))

(defmacro defmulti- [n dispatch]
  (let [nm (with-meta n {:private true})]
    `(defmulti ~nm ~dispatch)))
