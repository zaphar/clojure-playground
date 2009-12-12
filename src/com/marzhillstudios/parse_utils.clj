(ns #^{:doc "Parse utilities for parsing a sequence with some base parse
             matchers for parsing strings.
             
             Example: simple file format parser
             (def header
               (optional
                 (list-match (match-ignore \"--\") (until (match-ignore \"--\"))
                             (optional (repeated (space)))
                             (repeated-n (exact \\n) 3))))
             
             (def body (repeated (until \\n)))
             (def file (list-match header body))
             (def parser (grammar file))
             (defn parse-file [s] (apply-grammar parser s))
             "}
  com.marzhillstudios.parse-utils
  (:gen-class)
  (:use [com.marzhillstudios.test.tap :only [test-tap is ok]]
     [clojure.contrib.core :only [seqable?]]
     [clojure.contrib.str-utils :only [re-sub]]
     [com.marzhillstudios.util :only [defmulti-]]))

(defn- mk-leaf
  ([] (mk-leaf ())) 
  ([nm] (cons nm ())))

(defn apply-grammar
  "Apply a Grammar to a sequence. and get the resulting AST."
  [grammar s]
  ; TODO(jwall): should I care if the grammar didn't consume entire sequence?
  (:tree (grammar s)))

; Full Grammar constructor
(defn grammar
  "Returns a function that will parse a sequence
   using the provided parse-tree."
  [parse-tree] (fn [s] (parse-tree s)))

; Grammar function constructors

; Matcher modifiers
(defn match-ignore
  "Returns a function that matches/consumes the matchers
   and returns empty list if it matched. The function returns nil
   if the match fails."
  [matcher] (fn [s] (let [match (matcher s)]
                              (cond (nil? match) nil
                                :else {:tree (mk-leaf ()) :rest (:rest match)}))))

(defn forward-match
  "Returns a function that does a forward match but does not consume
   the matched tokens. The function will return nil if the match fails."
  [matcher] (fn [s] (let [match (matcher s)]
                              (cond (nil? match) nil
                                :else {:tree (mk-leaf) :rest (seq s)}))))

(defn optional
  "Returns a function that never returns nil. If provided matcher matches
   then the function returns the match. If provided matcher does not match
   then the function returns the empty list."
  [matcher] (fn [s] (let [match (matcher s)]
                              (cond (nil? match) {:tree (mk-leaf) :rest (seq s)}
                                :else match))))

; Matcher combinators

; TODO(jwall): backtracking matchers?

(defn- list-match-of
  ([acc matchers s]
   (cond (empty? matchers) {:tree (mk-leaf acc) :rest s}
     (empty? s) nil
     :else (let [matcher (first matchers)
                 token? (matcher s)]
             (cond (nil? token?) nil
               :else (recur (conj acc (:tree token?))
                            (drop 1 matchers)
                            (:rest token?))))))
  ([matchers s] (list-match-of [] matchers s)))
(defn list-match
  "Returns a function that matches/consumes a list of matchers.
   If entire list does not match the match fails and the function
   returns nil."
  [& l]
  (fn [s] (list-match-of l s)))

(defn- first-match-of [matchers s]
  (some (fn [m] (m s)) matchers))
(defn first-of
  "Returns a function that matches consumes the first of the matchers
   to match."
  [& matchers]
  (fn [s] (first-match-of matchers s)))

(defn- any-of
  ([matchers s]
   (let [candidate ((first matchers) s)]
     (cond (nil? candidate) (recur (drop 1 matchers) s)
       :else candidate))))
(defn any
  "Returns a function that returns the first match of any of
  the provided matchers."
  [& matchers] (fn [s] (any-of matchers s)))

(defn- repeated-match-n
  ([acc matcher n s]
    (let [matched (matcher s)
          cnt (inc (:count acc))]
      (cond
        (nil? matched) {:tree (mk-leaf (:tree acc)) :rest s}
        (= cnt n) {:tree (mk-leaf (conj (:tree acc)
                                          (:tree matched)))
                   :rest (:rest matched)}
        :else (recur {:tree (conj (:tree acc) (:tree matched))
                          :rest ""}
                          matcher n (:rest matched)))))
  ([matcher n s]
    (let [matched (matcher s)]
      (cond (nil? matched) nil
        :else (repeated-match-n {:tree [(:tree matched)] :rest "" :count 1}
                          matcher n (:rest matched))))))
(defn repeated-n
  "Returns a function that matches a matcher up to n times."
  [matcher n] (fn [s] (repeated-match-n matcher n s)))

(defn- repeated-match
  ([acc matcher s]
    (let [matched (matcher s)]
      (cond (nil? matched) {:tree (mk-leaf (:tree acc)) :rest s}
        :else (recur {:tree (conj
                               (:tree acc)
                               (:tree matched))
                      :rest ""}
                     matcher (:rest matched)))))
  ([matcher s]
    (let [matched (matcher s)]
      (cond (nil? matched) nil
        :else (repeated-match {:tree [(:tree matched)] :rest ""}
                          matcher (:rest matched))))))
(defn repeated [matcher]
  "Returns a function that matches a matcher greedily."
  (fn [s] (repeated-match matcher s)))

(defn annotated
  "Returns a function that annotates a provided base matcher. If the provided
   matcher does not match then the function returns nil."
  [annotation matcher]
    (fn [s]
      (let [match (matcher s)]
        (cond (nil? match) nil
          :else {:tree (mk-leaf (hash-map annotation (:tree match)))
                 :rest (:rest match)}))))

; Base matchers
(defn space []
  "Returns a function that matches/consumes exactly one space."
  (exact " "))

; TODO(jwall): linefeed, tab, carriage return

(defn- exact-token-maybe-fn
  ([token s] (exact-token-maybe-fn "" token s))
  ([acc token s]
    (cond
      (empty? token) {:tree (mk-leaf acc) :rest s}
      (empty? s) nil
      :else (let [sc (first s)
                  tc (first token)]
              (cond
                (= sc tc) (recur (str acc sc) (drop 1 token) (drop 1 s))
                :else nil)))))
(defmulti- exact-token-maybe (fn [token s] (seqable? token)))
(defmethod exact-token-maybe true
  ([token s] (exact-token-maybe-fn token s)))
(defmethod exact-token-maybe false
  ([token s] (exact-token-maybe-fn (cons token ()) s)))
(defn exact
  "Returns a function that reads a token from a sequence.
   Returns token and rest of seq if matched, nil if no match."
  [token] (partial exact-token-maybe token))

; TODO(jwall): add support for token either string or a matcher.
(defn- until-token-maybe-fn
  ([token s] (until-token-maybe-fn "" token s))
  ([acc token s] (let [token? (token s)]
                   (cond (empty? s) nil
                     (nil? token?) (recur (str acc (first s))
                                          token (drop 1 s))
                     :else {:tree (mk-leaf [(mk-leaf (str acc))
                                              (:tree token?)])
                            :rest (:rest token?)}))))
(defmulti- until-token-maybe (fn [t s] (ifn? t)))
(defmethod until-token-maybe true
  [token s] (until-token-maybe-fn token s))
(defmethod until-token-maybe false
  [token s] (until-token-maybe-fn (exact token) s))
(defn until 
  "Returns a function that reads up to a token from a sequence.
   Returns portion of the sequence that matched and the rest. nil if no match."
  [token] (partial until-token-maybe token))

; TODO(jwall): regex matcher function
(defmulti- re-match-of (fn [p _]
                         (instance? java.util.regex.Pattern p)))
(defmethod re-match-of true
  [pattern s] (let [match (re-find (re-matcher pattern s))]
                (cond (nil? match) nil
                  :else {:tree (mk-leaf (cond (seq? match) (drop 1 match)
                                   :else match))
                         :rest (seq (re-sub pattern "" s))})))
(defn re-match
  "Returns a function that matches and consumes a regex defining the token.
   If there are groups then the final grouping is the token returned.
   The function returns nil if the regex does not match"
  [pattern]
  (fn [s] (re-match-of pattern s)))

(defn test-suite []
  (test-tap 23
            (is {:tree (mk-leaf "foo") :rest (seq " bar")}
                (exact-token-maybe "foo" "foo bar"))
            (is {:tree (mk-leaf ";") :rest (seq "foo bar")}
                ((exact \;) ";foo bar"))
            (is {:tree (mk-leaf "foo") :rest (seq " bar")}
                ((re-match #"foo") "foo bar"))
            (is nil
                ((re-match #"foo") "fo bar"))
            (is {:tree (mk-leaf [(mk-leaf "foo ")
                         (mk-leaf "bar")])
                 :rest ()}
                (until-token-maybe "bar" "foo bar"))
            (is {:tree (mk-leaf [(mk-leaf "foo ")
                         (mk-leaf "bar")])
                 :rest ()}
                (until-token-maybe (exact "bar") "foo bar"))
            (is {:tree (mk-leaf {:foo (mk-leaf "foo")})
                 :rest (seq " bar")}
                ((annotated :foo (exact "foo")) "foo bar"))
            (is {:tree (mk-leaf "foo")  :rest (seq " bar")}
                (any-of [(exact "baz")
                         (until "bork")
                         (exact "foo")]
                        "foo bar"))
            (is {:tree (mk-leaf [(mk-leaf "foo ")
                         (mk-leaf "bar")])
                 :rest ()}
                ((any (exact "baz")
                      (until "bar")
                      (exact "foo"))
                       "foo bar"))
            (is {:tree (mk-leaf [(mk-leaf "foo")
                                   (mk-leaf "foo")])
                 :rest (seq " bar")}
                (repeated-match (exact "foo") "foofoo bar"))
            (is {:tree (mk-leaf [(mk-leaf "foo")
                                   (mk-leaf "foo")])
                 :rest (seq " bar")}
                ((repeated (exact "foo")) "foofoo bar"))
            (is {:tree (mk-leaf " ") :rest (seq "bar")}
                ((space) " bar"))
            (is {:tree (mk-leaf [(mk-leaf "foo")
                                   (mk-leaf "foo")])
                 :rest ()}
                ((repeated-n (exact "foo") 2) "foofoo"))
            ; not sure this is the correct behaviour?
            (is {:tree (mk-leaf [(mk-leaf "foo")]) :rest ()}
                ((repeated-n (exact "foo") 2) "foo"))
            (is {:tree (mk-leaf [(mk-leaf "foo")
                                   (mk-leaf "foo")])
                 :rest (seq " bar")}
                ((repeated-n (exact "foo") 2) "foofoo bar"))
            (is {:tree (mk-leaf [(mk-leaf "foo")
                                   (mk-leaf "foo")])
                 :rest (seq "foo bar")}
                ((repeated-n (exact "foo") 2) "foofoofoo bar"))
            (is {:tree (mk-leaf "foo")
                 :rest (seq " bar")}
                ((first-of (exact "bar") (exact "foo")) "foo bar"))
            (is {:tree (mk-leaf "foo") :rest (seq " bar")}
                ((optional (exact "foo")) "foo bar"))
            (is {:tree (mk-leaf) :rest (seq "fo bar")}
                ((optional (exact "foo")) "fo bar"))
            (is {:tree (mk-leaf) :rest (seq "foo bar")}
                ((forward-match (exact "foo")) "foo bar"))
            (is nil
                ((forward-match (exact "foo")) "fo bar"))
            (is {:tree (mk-leaf) :rest (seq (seq " bar"))}
                ((match-ignore (exact "foo")) "foo bar"))
            (is nil
                ((match-ignore (exact "foo")) "fo bar"))
            ))

