(ns
  com.marzhillstudios.path
  (:gen-class)
  (:import (java.lang IllegalArgumentException))
  (:require [com.marzhillstudios.util :as util]
     [com.marzhillstudios.list.util :as lu]
     [clojure.contrib.str-utils2 :as su]
     [clojure.contrib.core :as ccc]))

(defn path-type [] ::path)

(defn path-str
  [path] (su/join "/" path))

(defmulti absolute? type)
(defmethod absolute? java.lang.String [s]
  (= (first s) \/))
(defmethod absolute? clojure.lang.Seqable [s]
  (= (first s) :root))

(defmulti mk-path type)
(defmethod mk-path java.lang.String [p]
  (util/add-type
    ::path
    (cond (absolute? p) (cons :root (drop 1 (seq (.split p "/"))))
      :else (seq (.split p "/")))))
(defmethod mk-path clojure.lang.Seqable [s]
  (let [p (seq s)
        c (first p)]
    (util/add-type ::path p)))

(defmulti basename type)
(defmethod basename java.lang.String
  [path] (basename (mk-path path)))
(defmethod basename (path-type)
  [path] (last path))

(defmulti dirname type)
(defmethod dirname java.lang.String
  [path] (dirname (mk-path path)))
(defmethod dirname (path-type)
  [path] (path-str (drop-last path)))

(defmulti resolve-path #([(type %1) (type %2)]))
(defmethod resolve-path [java.lang.String java.lang.String]
  [p anchor] (resolve-path (mk-path p) (mk-path anchor)))
(defmethod resolve-path [java.lang.String  (path-type)]
  [p anchor] (resolve-path (mk-path p) anchor))
(defmethod resolve-path [(path-type) java.lang.String]
  [p anchor] (resolve-path p (mk-path anchor)))
(defmethod resolve-path [(path-type) (path-type)]
  [p anchor] ())

(defn- normalize-path [prefix rst]
  (filter #(not (nil? %))
          (lazy-cat
            prefix
            ; make a starter a path object and foldl across it
            (lu/foldl
              [] (fn [acc nxt]
                   (let [result (cond (= nxt "..")
                                  (cond (>= (count acc) 1)
                                     (vec (drop-last acc))
                                    :else (throw (IllegalArgumentException.
                                                 "Malformed path")))
                                  (= nxt ".") acc 
                                  :else (conj acc nxt))]
                     result))
              rst))))

(defn normalize-relative-path [p]
  (when (absolute? p)
    ; can't be absolute path
    (throw (IllegalArgumentException. "Path can't be an absolute path")))
  (let [path (mk-path p)
        ddprefix (take-while #(= % "..") path)
        sdprefix (take-while #(= % ".") path)
        rst (drop (count (concat ddprefix sdprefix)) path)]
    ; prefix .'s get collapsed
    ; all prefix ..'s stay
    (normalize-path (lazy-cat ddprefix (cons (first sdprefix) ()))
                    rst)))

(defn normalize-absolute-path [p]
  (when (not (absolute? p))
    ; can't be relative path
    (throw (IllegalArgumentException. "Path must be an absolute path")))
  (let [path (mk-path p)
        root (first path)
        rst (drop 1 path)]
  ; make a starter a path object and foldl across it
    (normalize-path [root] rst)))

(defn make-absolute-path [p anchor]
  (when (not (absolute? anchor))
    ; anchor must be an absolute path
    (throw (IllegalArgumentException. "Anchor must be an absolute path")))
  (mk-path (concat anchor p)))

