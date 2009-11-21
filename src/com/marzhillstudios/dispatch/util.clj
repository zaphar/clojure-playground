(ns
  #^{:author "Jeremy Wall"
     :doc "This library contains useful functions to be used
           as dispatch functions for multi-methods."}
  com.marzhillstudios.dispatch.util
  (:gen-class))

(defn 
  #^{:doc "Returns type of anything."}
  just [x] (type x))

(defn
  #^{:doc "Returns type of anything or nil."}
  maybe
  ([] nil)
  ([x] (just x)))

(defn
  #^{:doc "Returns type t or nil"}
  maybe-type
  [t] (fn [x] (maybe (cond (= (type x) t) x))))
