; Copyright (c) Jeremy Wall. All rights reserved.
; This software is available under the terms of the 
; Artistic License 2.0 
;   (http://www.opensource.org/licenses/artistic-license-2.0.php)
; By using this software in any fashion you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
(ns path.uri
  (:gen-class)
  (:import (java.io PushbackReader StringReader)
           (java.lang IllegalStateException))
  (:use test.tap)
  (:use dispatch.util))

(derive java.lang.String ::string)
(derive clojure.lang.LazilyPersistentVector ::list)
(derive clojure.lang.Cons ::list)
(derive java.io.Reader ::io)

(declare mk-uri-struct read-scheme read-authority read-path
  read-query read-frag get-char get-stream drop-n-chars
  parse-uri-string read-chars read-to-char read-stream
  read-user-pass-rest read-domain-port)

(defstruct uri :scheme :authority :path :query :fragment)
(defstruct uri-authority :user :pass :domain :port)

(defmulti mk-uri type)
(defmethod mk-uri ::string [s]
  (parse-uri-string s))
(defmethod mk-uri ::list [l]
  (mk-uri-struct l))

(defn- parse-uri-string [s]
  (with-in-str s
      (mk-uri-struct (read-scheme)
                      (read-authority)
                      (read-path)
                      (read-query)
                      (read-frag))))

(defn- mk-uri-struct [scheme
                      authority
                      path
                      query
                      fragment]
  (struct-map uri
    :scheme scheme
    :authority authority
    :path path :query query
    :fragment fragment))

(defn- read-frag
  ([] (read-frag (get-stream)))
  ([s] (read-stream s)))

(defn- read-query
  ([] (read-query (get-stream)))
  ([s] (map
         (fn [x] (vec (.split x "=")))
         (vec (.split (read-to-char \# s) "&")))))

(defn- read-path
  ([] (read-path (get-stream)))
  ([s] (read-to-char \? s)))

(defn read-user-pass-rest
  [s] (let [v (vec (.split s "@"))]
            (cond
              (> (.size v) 1) v
              :else [nil (first v)])))

(defn read-domain-port
  [s] (let [v (.split s ":")
            domain (first v)
            port (drop 1 v)]
        [domain (cond (= port ()) nil
                  :else (Integer/valueOf (first port)))]))

(defn read-user
  [s] (first (.split s ":")))

(defn read-pass
  [s] (let [v (vec (.split s ":"))]
        (cond
          (> (.size v) 1) (nth v 1)
          :else nil)))

(defn- read-authority
  ([] (read-authority (get-stream)))
  ([s] (let [auth (read-to-char \/ s)
             user-pass-rest (read-user-pass-rest auth)
             user-pass (first user-pass-rest)
             domain-port (read-domain-port (nth user-pass-rest 1))
             domain (first domain-port)
             port (nth domain-port 1)
             user (cond (nil? user-pass) nil
                    :else (read-user user-pass))
             pass (cond (nil? user-pass) nil
                    :else (read-pass user-pass))]
         (struct-map uri-authority
                     :user user
                     :pass pass
                     :domain domain
                     :port port))))

(defn- read-scheme
  ([] (read-scheme (get-stream)))
  ([s] (let [c (get-char s)]
        (cond 
          (nil? c) ""
          (= c \:) (cond
            (= "://" (str c (read-chars 2 s))) ""
            :else (throw "Malformed scheme in uri"))
          (not (= c \:)) (let [scheme (str c (read-scheme s))]
                               scheme)))))

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

(defmulti get-stream maybe)
(defmethod get-stream nil [] *in*)
(defmethod get-stream ::io [in] in)
(defmethod get-stream ::string [s] (PushbackReader. (StringReader. s)))

(defn test-suite []
    (test-tap 20
      (nil? (get-char (get-stream "")))
      (is \f (get-char (get-stream "f")))
      (is "123" (read-chars 3 (get-stream "1234")))
      (is "foo" (read-scheme (get-stream "foo://")))
      (is nil (first (read-user-pass-rest "bar.com")))
      (is ["user:pass" "bar.com"]
          (read-user-pass-rest "user:pass@bar.com"))
      (is "user" (read-user "user:pass"))
      (is "pass" (read-pass "user:pass"))
      (is nil (read-pass "user"))
      (is ["bar.com" 80] (read-domain-port "bar.com:80"))
      ; TODO(jwall): need to handle : without port number properly
      (is ["bar.com" nil] (read-domain-port "bar.com"))
      (is (struct-map uri-authority
             :user "user"
             :pass "pass"
             :domain "bar.com"
             :port 80)
          (with-in-str "user:pass@bar.com:80/blah" (read-authority)))
      (is "path/to/some" (read-path (get-stream "path/to/some?q=1")))
      (is [["q" "1"]] (read-query (get-stream "q=1#frag")))
      (is "frag" (read-frag (get-stream "frag")))
      (is [[["q" "1"]] "frag"] (with-in-str "q=1#frag" [(read-query) (read-frag)]))
      (is "foo" (with-in-str "foo://bar.com/blah" (read-scheme)))
      (is "foo" (:scheme (mk-uri "foo://bar.com/blah?q=1#frag")))
      (is (struct-map uri-authority
             :user "user"
             :pass "pass"
             :domain "bar.com"
             :port 80) (:authority (mk-uri "foo://user:pass@bar.com:80/blah?q=1#frag")))
      (is "blah" (:path (mk-uri "foo://bar.com/blah?q=1#frag")))
      (is [["q" "1"]] (:query (mk-uri "foo://bar.com/blah?q=1#frag")))
      (is "frag" (:fragment (mk-uri "foo://bar.com/blah?q=1#frag")))
      (is ["foo"
           (struct-map uri-authority
             :user "user"
             :pass "pass"
             :domain "bar.com"
             :port 123)
           "blah"
           [["q" "1"]]
           "frag"]
          (with-in-str "foo://user:pass@bar.com:123/blah?q=1#frag"
                [(read-scheme)
                 (read-authority)
                 (read-path)
                 (read-query)
                 (read-frag)]))))
