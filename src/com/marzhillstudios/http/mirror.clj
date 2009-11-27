(ns
  com.marzhillstudios.http.mirror
  (:require [clojure.contrib.duck-streams :as ds]
     [com.marzhillstudios.path.uri :as uri]
     [com.marzhillstudios.list.util :as lu]
     [com.marzhillstudios.util :as util])
  (:use [com.marzhillstudios.test.tap :only [test-tap ok is]]
     [com.marzhillstudios.http.stream :only [get-http-stream]]))

(defstruct file-spec :contents :urls)
(defn- mk-file-spec [contents urls]
  (util/add-type ::file-spec
            (struct-map file-spec
              :contents contents
              :urls urls)))

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
     (mk-file-spec c url-list)))

(defn- consume-stream [s]
  (lu/foldl (mk-file-spec [] [])
            do-line s))

(defn process-stream [uri]
  (consume-stream (get-http-stream uri)))

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
