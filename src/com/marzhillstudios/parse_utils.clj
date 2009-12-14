(ns #^{:doc "Parse utilities for parsing a sequence with some base parse
             matchers for parsing strings.
             
             Example: simple file format parser
             (def header
               (optional
                 (list-match (ignore \"--\") (until (ignore \"--\"))
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
  ([nm] (cond
          (or (vector? nm)
              (seq? nm))(let [branch (filter #(not (= % ())) nm)]
                          (cond (= (count branch) 1) (first branch)
                            :else branch))
          :else nm)))

(defstruct ast-state :tree :rest)
(defn mk-ast-state
  "Constructs a state map for the grammar matchers to use."
  ([tree rst & opts]
    (apply struct-map ast-state :tree tree :rest rst opts))
  ([tree rst]
    (struct-map ast-state :tree tree :rest rst)))

(defn apply-grammar
  "Apply a Grammar to a sequence. and get the resulting AST.
   AST: {tree rest}"
  [grammar s]
  (let [parsed (grammar s)]
    {(:tree parsed)
     (:rest parsed)}))

; Full Grammar constructor
(defn grammar
  "Returns a function that will parse a sequence
   using the provided parse-tree."
  [parse-tree] (fn [s] (parse-tree s)))

; Grammar matcher constructors

; Matcher modifiers
(defn- ignore-fn
  "Returns a function that matches/consumes the matchers
   and returns empty list if it matched. The function returns nil
   if the match fails."
  [matcher] (fn [s] (let [match (matcher s)]
                              (cond (nil? match) nil
                                :else (mk-ast-state (mk-leaf)
                                                   (:rest match))))))
(defmulti ignore ifn?)
(defmethod ignore true
  [token] (ignore-fn token))
(defmethod ignore false
  [token] (ignore-fn (exact token)))

(defn- forward-match-fn
  "Returns a function that does a forward match but does not consume
   the matched tokens. The function will return nil if the match fails."
  [matcher] (fn [s] (let [match (matcher s)]
                              (cond (nil? match) nil
                                :else (mk-ast-state (mk-leaf) 
                                                   (seq s))))))
(defmulti forward-match ifn? )
(defmethod forward-match true
  [token] (forward-match-fn token))
(defmethod forward-match false
  [token] (forward-match-fn (exact token)))

(defn- optional-fn
  "Returns a function that never returns nil. If provided matcher matches
   then the function returns the match. If provided matcher does not match
   then the function returns the empty list."
  [matcher] (fn [s] (let [match (matcher s)]
                      (cond (nil? match) (mk-ast-state (mk-leaf)
                                                       (seq s))
                        :else match))))
(defmulti optional ifn?)
(defmethod optional true
  [token] (optional-fn token))
(defmethod optional false
  [token] (optional-fn (exact token)))

(defn annotated-fn
  "Returns a function that annotates a provided base matcher. If the provided
   matcher does not match then the function returns nil."
  [annotation matcher]
    (fn [s]
      (let [match (matcher s)]
        (cond (nil? match) nil
          :else (mk-ast-state (mk-leaf (hash-map annotation (:tree match)))
                             (:rest match))))))
(defmulti annotated (fn [annotation t] (ifn? t)))
(defmethod annotated true
  [annotation token] (annotated-fn annotation token))
(defmethod annotated false
  [annotation token] (annotated-fn annotation (exact token)))

; Matcher combinators
(defn- list-match-of
  ([acc matchers s]
   (cond (empty? matchers) (mk-ast-state (mk-leaf acc) s)
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
   (let [matcher (first matchers)]
     (cond (nil? matcher) nil
       :else (let [candidate (matcher s)]
               (cond (nil? candidate) (recur (drop 1 matchers) s)
                 :else candidate))))))
(defn any
  "Returns a function that returns the first match of any of
  the provided matchers."
  [& matchers] (fn [s] (any-of matchers s)))

(defn- repeated-match-n
  ([acc matcher n s]
    (let [matched (matcher s)
          cnt (inc (:count acc))]
      (cond
        (nil? matched) (mk-ast-state (mk-leaf (:tree acc)) s)
        (= cnt n) (mk-ast-state (mk-leaf (conj (:tree acc)
                                              (:tree matched)))
                    (:rest matched))
        :else (recur (mk-ast-state (conj (:tree acc) (:tree matched))
                                  "")
                          matcher n (:rest matched)))))
  ([matcher n s]
    (let [matched (matcher s)]
      (cond (nil? matched) nil
        :else (repeated-match-n (mk-ast-state [(:tree matched)] "" :count 1)
                          matcher n (:rest matched))))))
(defn repeated-n
  "Returns a function that matches a matcher up to n times."
  [matcher n] (fn [s] (repeated-match-n matcher n s)))

(defn- repeated-match
  ([acc matcher s]
    (let [matched (matcher s)]
      (cond (nil? matched) (mk-ast-state (mk-leaf (:tree acc)) s)
        :else (recur (mk-ast-state (conj (:tree acc)
                                        (:tree matched)) "")
                     matcher (:rest matched)))))
  ([matcher s]
    (let [matched (matcher s)]
      (cond (nil? matched) nil
        :else (repeated-match (mk-ast-state [(:tree matched)] "")
                          matcher (:rest matched))))))
(defn repeated [matcher]
  "Returns a function that matches a matcher greedily."
  (fn [s] (repeated-match matcher s)))

(defn merge-annotations
  "Returns a function that matches/consumes a list of annotation matchers
   and merges the annotations into a single map. Returns nil if there was
   no match."
  [& annotations]
  (fn [s] (let [annotated-list (list-match-of annotations s)]
            (cond (nil? annotated-list) nil
              :else(mk-ast-state
              (mk-leaf (reduce (fn [m1 m2] (merge m1 m2))
                               (:tree annotated-list)))
              (:rest annotated-list))))))

; Base matchers
(defn space []
  "Returns a function that matches/consumes exactly one space."
  (exact \space ))
(defn tab []
  (exact \tab))

(defn cr []
  (exact \return ))
(defn lf []
  (exact \newline))
(defn crlf []
  (list-match (cr) (lf)))

(defn whitespace []
  (any (space) (tab) (crlf) (cr) (lf)))

; TODO(jwall): linefeed, tab, carriage return

(defn- exact-token-maybe-fn
  ([token s] (exact-token-maybe-fn "" token s))
  ([acc token s]
    (cond
      (empty? token) (mk-ast-state (mk-leaf acc) s)
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
                     :else (mk-ast-state (mk-leaf [(mk-leaf (str acc))
                                                   (:tree token?)])
                              (:rest token?))))))
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
                  :else (mk-ast-state (mk-leaf
                                        (cond (or (seq? match)
                                                  (vector? match)) (last match)
                                              :else match))
                           (seq (re-sub pattern "" s))))))
(defn re-match
  "Returns a function that matches and consumes a regex defining the token.
   If there are groups then the final grouping is the token returned.
   The function returns nil if the regex does not match"
  [pattern]
  (fn [s] (re-match-of pattern (reduce #(str %1 %2) s))))

(defn test-suite []
  (test-tap 28
            (is "foo" (mk-leaf ["foo" ()]))
            (is "foo" (mk-leaf '("foo" ())))
            (is {:tree (mk-leaf "foo") :rest (seq " bar")}
                (exact-token-maybe "foo" "foo bar"))
            (is {:tree (mk-leaf ";") :rest (seq "foo bar")}
                ((exact \;) ";foo bar"))
            (is {:tree (mk-leaf "foo") :rest (seq " bar")}
                ((re-match #"foo") "foo bar"))
            (is {:tree (mk-leaf "foo") :rest (seq " bar")}
                ((re-match #"(foo)") "foo bar"))
            (is nil
                ((re-match #"foo") "fo bar"))
            (is nil
                ((re-match #"^foo") "barfoo bar"))
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
                ((ignore (exact "foo")) "foo bar"))
            (is nil
                ((ignore (exact "foo")) "fo bar"))
            (is (mk-ast-state {:foo "oof" :bar "rab"} ())
                ((merge-annotations
                  (annotated :foo (exact "oof"))
                  (annotated :bar (exact "rab"))) "oofrab"))
            ))

