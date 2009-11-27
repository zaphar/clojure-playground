; Copyright (c) Jeremy Wall. All rights reserved.
; This software is available under the terms of the 
; Artistic License 2.0 
;   (http://www.opensource.org/licenses/artistic-license-2.0.php)
; By using this software in any fashion you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
(ns com.marzhillstudios.path.uri
  (:gen-class)
  (:import (java.io PushbackReader StringReader)
           (java.lang IllegalStateException))
  (:use com.marzhillstudios.test.tap)
  (:use com.marzhillstudios.dispatch.util)
  (:use com.marzhillstudios.list.util)
  (:use clojure.contrib.str-utils))

; TODO(jwall): all pieces should be optional
(derive java.lang.String ::string)
(derive clojure.lang.LazilyPersistentVector ::list)
(derive clojure.lang.Cons ::list)
(derive java.io.Reader ::io)

(declare mk-uri-struct read-scheme read-authority read-path
  read-query read-frag get-char get-stream drop-n-chars
  parse-uri-string read-chars read-to-char read-stream
  read-user-pass-rest read-domain-port authority-to-string
  query-to-string fragments-to-string)

(defstruct uri :scheme :authority :path :query :fragment)
(defstruct uri-authority :user :pass :domain :port)

(defn uri-to-string [u]
  (str (:scheme u) "://" (authority-to-string (:authority u))
    (:path u) (query-to-string (:query u))
       (fragments-to-string (:fragment u))))

(defmulti mk-uri type)
(defmethod mk-uri ::string [s]
  (parse-uri-string s))
(defmethod mk-uri ::list [l]
  (mk-uri-struct l))

(defn- fragments-to-string [s]
    (cond (nil? s) ""
      :else (str "#" s)))

(defn- query-to-string [q]
    (let [query (foldl "?" (fn [acc pair]
                (str acc (first pair)
                     "=" (nth pair 1) "&")) q)]
      (chop query)))

(defn- authority-to-string [auth]
  (let [user (:user auth)
        pass (:pass auth)
        user-pass (str (cond (nil? user) ""
                              :else user)
                       (cond (nil? pass) ""
                         :else (str ":" pass)))
        domain (:domain auth)
        port (:port auth)]
    (str (cond (= user-pass "") ""
           :else (str user-pass "@"))
         domain (cond (nil? port) ""
                  :else (str ":" port)))))

(defn- parse-uri-string [s]
      (let [scheme (read-scheme s)
            authority (read-authority s)
            path (read-path s)
            query (read-query s)
            frag (read-frag s)]
        (mk-uri-struct scheme
                       authority
                       path
                       query
                       frag)))

(defn mk-uri-struct [scheme
                      authority
                      path
                      query
                      fragment]
  (with-meta (struct-map uri
    :scheme scheme
    :authority authority
    :path path :query query
    :fragment fragment) {:type ::uri}))

(def frag-pattern (re-pattern "#(.+)"))
(defn- read-frag
  ([s] (let [match (re-find (re-matcher frag-pattern s))]
         (cond (nil? match) nil
           :else (last match)))))

(def query-pattern (re-pattern "\\?([^#]+).*"))
(defn- read-query
  ([s] (let [match (re-find (re-matcher query-pattern s))]
         (cond (nil? match) ()
           :else (map (fn [x] (vec (.split x "=")))
                      (vec (.split (last match) "&")))))))

(def path-pattern (re-pattern "^([a-zA-Z][^:]*://)?([^/]+)?(/?[^#\\?]*)"))
(defn- read-path
  ([s] (let [match (re-find (re-matcher path-pattern s))]
         (cond (nil? match) nil
           (nil? (nth match 1)) (first match)
           :else (last match)))))

(defn- read-user-pass-rest
  [s] (let [v (vec (.split s "@"))]
            (cond
              (> (.size v) 1) v
              :else [nil (first v)])))

(defn- read-domain-port
  [s] (let [v (.split s ":")
            domain (first v)
            port (drop 1 v)]
        [domain (cond (= port ()) nil
                  :else (Integer/valueOf (first port)))]))

(defn- read-user
  [s] (first (.split s ":")))

(defn- read-pass
  [s] (let [v (vec (.split s ":"))]
        (cond
          (> (.size v) 1) (nth v 1)
          :else nil)))

(def authority-pattern (re-pattern "://([^/]+)/?.*"))
(defn- read-authority
  ([s] (let [authmatch (re-find (re-matcher authority-pattern s))
             auth (cond (nil? authmatch) (let [new-match (re-find (re-matcher (str "://" s)))]
                                               (cond (nil? new-match) nil
                                                 :else (last new-match)))
                    :else (nth authmatch 1))]
         (cond (nil? auth) nil
           :else (let [user-pass-rest (read-user-pass-rest auth)
                       user-pass (first user-pass-rest)
                       domain-port (read-domain-port (nth user-pass-rest 1))
                       domain (first domain-port)
                       port (nth domain-port 1)
                       [user pass] (cond (nil? user-pass) [nil nil]
                                     :else [(read-user user-pass)
                                            (read-pass user-pass)])]
                 (struct-map uri-authority
                             :user user
                             :pass pass
                             :domain domain
                             :port port))))))

(def scheme-pattern (re-pattern "([a-zA-Z][a-zA-z0-0.+]*):"))
(defn- read-scheme
  ([s] (let [match (re-find (re-matcher scheme-pattern s))]
         (cond (nil? match) nil
           :else (nth match 1)))))

(defn- read-stream [s]
  (let [c (get-char s)]
    (cond
      (nil? c) ""
      :else (str c (read-stream s)))))

(defn- read-to-char [i s]
  (let [c (get-char s)]
        (cond 
          (nil? c) ""
          (= c i) ""
          :else (let [token (str c (read-to-char i s))]
                  token))))

(defn- get-char [s] (let [i (.read s)]
                      (cond (not (= i -1)) (char i)
                            (= i -1) nil)))

(defn- read-chars [n s] 
  (cond
    (> n 0) (str (get-char s) (read-chars (dec n) s))
    :else ""))

(defmulti #^{:private true } get-stream maybe)
(defmethod get-stream nil [] *in*)
(defmethod get-stream ::io [in] in)
(defmethod get-stream ::string [s] (PushbackReader. (StringReader. s)))

(defn test-suite []
    (test-tap 23
      (nil? (get-char (get-stream "")))
      (is \f (get-char (get-stream "f")))
      (is "123" (read-chars 3 (get-stream "1234")))
      (is "foo" (read-scheme "foo://"))
      (is nil (read-scheme "foo//"))
      (is nil (first (read-user-pass-rest "bar.com")))
      (is ["user:pass" "bar.com"]
          (read-user-pass-rest "user:pass@bar.com"))
      (is "user" (read-user "user:pass"))
      (is "pass" (read-pass "user:pass"))
      (is nil (read-pass "user"))
      (is ["bar.com" 80] (read-domain-port "bar.com:80"))
      (is ["bar.com" nil] (read-domain-port "bar.com"))
      (is (struct-map uri-authority
             :user "user"
             :pass "pass"
             :domain "bar.com"
             :port 80)
           (read-authority "http://user:pass@bar.com:80/blah"))
      (is "path/to/some" (read-path "path/to/some?q=1"))
      (is "path/to/some" (read-path "path/to/some#frag"))
      (is "path/to/some" (read-path "path/to/some?q=1#frag"))
      (is ["q" "1"] (first (read-query "?q=1#frag")))
      (is "frag" (read-frag "#frag"))
      (is [["q" "1"] "frag"] [(first (read-query "?q=1#frag"))
                                  (read-frag "?q=1#frag")])
      (is "foo" (:scheme (mk-uri "foo://bar.com/blah?q=1#frag")))
      (is (struct-map uri-authority
             :user "user"
             :pass "pass"
             :domain "bar.com"
             :port 80) (:authority (mk-uri "foo://user:pass@bar.com:80/blah?q=1#frag")))
      (is "/blah" (:path (mk-uri "foo://bar.com/blah?q=1#frag")))
      (is [["q" "1"]] (:query (mk-uri "foo://bar.com/blah?q=1#frag")))
      (is "frag" (:fragment (mk-uri "foo://bar.com/blah?q=1#frag")))
      (is "foo://user:pass@bar.com/blah?q=1#frag"
          (uri-to-string (mk-uri "foo://user:pass@bar.com/blah?q=1#frag")))
      (is "user:pass@foo.com:80", (authority-to-string  
                                    (struct-map uri-authority
                                      :user "user"
                                      :pass "pass"
                                      :domain "foo.com"
                                      :port 80)))
      (is ["foo"
           (struct-map uri-authority
             :user "user"
             :pass "pass"
             :domain "bar.com"
             :port 123)
           "/blah"
           '(["q" "1"])
           "frag"]
          (let [s "foo://user:pass@bar.com:123/blah?q=1#frag"]
                [(read-scheme s)
                 (read-authority s)
                 (read-path s)
                 (read-query s)
                 (read-frag s)]))
                 ))
