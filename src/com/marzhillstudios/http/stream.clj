(ns
  com.marzhillstudios.http.stream
  (:require
     [com.marzhillstudios.path.uri :as uri]
     [com.marzhillstudios.list.util :as lu]
     [clojure.contrib.http.agent :as ha])
  (:use [com.marzhillstudios.test.tap :only [test-tap ok is]]))

; TODO(jwall): make it use read-lines from the duck-streams lib?
(defmulti get-http-page type)
(defmethod get-http-page java.lang.String
  [s] (ha/stream (ha/http-agent s)))
(defmethod get-http-page :com.marzhillstudios.path.uri/uri
  [u] (get-http-page (uri/uri-to-string u)))
