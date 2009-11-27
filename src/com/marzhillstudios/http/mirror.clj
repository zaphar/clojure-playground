(ns
  com.marzhillstudios.http.mirror
  (:require [clojure.contrib.duck-streams :as ds]
     [com.marzhillstudios.path.uri :as uri]
     [com.marzhillstudios.list.util :as lu])
  (:use [com.marzhillstudios.test.tap :only [test-tap ok is]]
     [com.marzhillstudios.http.stream :only [get-http-stream]]))

(defstruct file-spec :contents :urls)

(def link-pattern (re-pattern  "(href|src)='(([^'])+)'|\"(([^\"])+)\""))
(defn- find-urls [s]
  (map (fn [match] (nth match 2))
       (re-seq link-pattern s)))

(defn- process-urls [ln]
  [(find-urls ln) ln])

(defn- do-line [fspec ln]
  (let [[urls line] (process-urls ln)
         c (concat (:contents fspec) [line])
         url-list (concat (:urls fspec) urls)]
     (struct-map file-spec :contents c :urls url-list)))

(defn- consume-file [s]
  (lu/foldl (struct-map file-spec :contents [] :urls [])
            do-line s))

(defn process-stream [uri]
  (consume-file (get-http-stream uri)))

(defn test-suite []
  (test-tap 4
            (is '("http://foo.bar/" "http://bar.com/")
                (find-urls
                  (str "fjkdla; fooo <a href='http://foo.bar/'></a> fjdkla;"
                       "fjkdla; fooo <img src='http://bar.com/'></a> fjdkla;")))
            (is "http://foo.bar/"
                (first (find-urls
                  (str "fjkdla; fooo <a href='http://foo.bar/'></a> fjdkla;"
                       "fjkdla; fooo <a href='http://bar.com/'></a> fjdkla;"))))
            (let [s (str "fjkdla; fooo <a href='http://foo.bar/'></a> fjdkla;"
                       "fjkdla; fooo <a href='http://bar.com/'></a> fjdkla;")]
              (is ['("http://foo.bar/" "http://bar.com/") s]
                (process-urls s)))
            (let [s (str "fjkdla; fooo <a href='http://foo.bar/'></a> fjdkla;"
                       "fjkdla; fooo <a href='http://bar.com/'></a> fjdkla;")
                  fspec (struct-map file-spec :contents '("")
                                    :urls '("http://foo.bar/"))]
              (is (struct-map file-spec :contents (seq ["" s])
                               :urls '("http://foo.bar/" "http://foo.bar/" "http://bar.com/"))
                (do-line fspec s)))
            ))
