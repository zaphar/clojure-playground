(ns
  com.marzhillstudios.http.mirror
  (:import (java.util.concurrent ScheduledThreadPoolExecutor))
  (:require [clojure.contrib.duck-streams :as ds]
     [clojure.contrib.str-utils :as su]
     [clojure.contrib.pprint :as pp]
     [com.marzhillstudios.path.uri :as uri]
     [com.marzhillstudios.list.util :as lu]
     [com.marzhillstudios.util :as util])
  (:use [com.marzhillstudios.test.tap :only [test-tap ok is]]
     [com.marzhillstudios.http.stream :only [get-http-stream]]))

(defstruct file-spec :contents :urls :name)
(defn- mk-file-spec [contents urls nm]
  (util/add-type ::file-spec
            (struct-map file-spec
              :contents contents
              :urls urls
              :name nm)))

(def link-pattern (re-pattern  "(((href|src)='([^']+)')|((href|src)=\"([^\"]+)))"))
;(defn apply-url-pattern [s]
;  (re-find (re-matcher link-pattern s)))
(defn- find-urls [s]
  (filter #(not (nil? %))
          (map (fn [match] (cond (nil? (last match)) (nth match 4)
                                       :else (last match)))
               (re-seq link-pattern s))))

; TODO(jwall): we need to resolve relative urls
; TODO(jwall): filter out non-http urls
(defn- process-urls [ln]
  [(find-urls ln) ln])

(defn- do-line [fspec ln]
  (let [[urls line] (process-urls ln)
         c (concat (:contents fspec) [line])
         url-list (concat (:urls fspec) urls)]
     (mk-file-spec c url-list (:name fspec))))

(defn- consume-stream [s uri]
  (lu/foldl (mk-file-spec [] [] uri)
            do-line s))

(defn- save-page [fspec] (do (println (str "saving: page "
                                      (:name fspec)))))

(defstruct site-spec :site :urls :pool)
(defn- mk-site-spec
  ([uri pool] (mk-site-spec uri pool []))
  ([uri pool urls] (struct-map site-spec
                          :site uri
                          :urls urls
                          :pool pool)))

(defn- processed-page? [site-spec url]
  (some #(= (su/re-sub #"/$" "" url)
            (su/re-sub #"/$" "" %))
        (:urls site-spec)))

(defn- update-url-list [site-spec url-list]
  (mk-site-spec (:site site-spec)
                (:pool site-spec)
                (concat (:urls site-spec) url-list)))

(defn process-page [uri a]
  (let [done? (processed-page? @a uri)]
    (pp/pprint done?)
    (when (not done?)
      (println (str "haven't processed page yet: " uri " processing now"))
      (let [fspec (consume-stream (get-http-stream uri) uri)]
        (save-page fspec) ;assume this is done then update url-list
        (send a update-url-list [uri])
        (doseq [url (:urls fspec)]
          (println (str "queuing job for " url))
          (.execute (:pool @a)
                    (process-page url a)))))
    done?))

(defn process-site
  ([uri] (process-site uri 5))
  ([uri size]
   (let [pool (ScheduledThreadPoolExecutor. size)
         a (agent (mk-site-spec uri pool))]
     (process-page uri a))))

(defn- mock-site-spec []
  (mk-site-spec "http://foo.com/"
                nil
                ["http://foo.com/test1/"
                 "http://foo.com/test2/"]))

(defn test-suite []
  (test-tap 6
            (is '("http://foo.bar/" "http://bar.com/")
                (find-urls
                  (str "fjkdla; fooo <a href='http://foo.bar/'></a> fjdkla;"
                       "fjkdla; fooo <img src='http://bar.com/'></a> fjdkla;")))
            (is "http://foo.bar/"
                (first (find-urls
                  (str "fjkdla; fooo <a href=\"http://foo.bar/\"></a> fjdkla;"
                       "fjkdla; fooo <a href='http://bar.com/'></a> fjdkla;"))))
            (let [s (str "fjkdla; fooo <a href='http://foo.bar/'></a> fjdkla;"
                       "fjkdla; fooo <a href='http://bar.com/'></a> fjdkla;")]
              (is ['("http://foo.bar/" "http://bar.com/") s]
                (process-urls s)))
            (let [s (str "fjkdla; fooo <a href='http://foo.bar/'></a> fjdkla;"
                       "fjkdla; fooo <a href='http://bar.com/'></a> fjdkla;")
                  fspec (mk-file-spec '("") '("http://foo.bar/")
                                      "http://foo.bar/")]
              (is (mk-file-spec (seq ["" s])
                                '("http://foo.bar/"
                                  "http://foo.bar/"
                                  "http://bar.com/")
                                "http://foo.bar/")
                (do-line fspec s)))
            (ok (processed-page? (mock-site-spec) "http://foo.com/test2"))
            (ok (processed-page? (mock-site-spec) "http://foo.com/test2/"))
            ))
