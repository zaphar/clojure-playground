(ns
  com.marzhillstudios.util
  (:gen-class)
  (:use clojure.contrib.pprint))

(defn print-class-path []
  (pprint (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))))
