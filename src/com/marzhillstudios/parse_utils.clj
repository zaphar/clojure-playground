(ns
  com.marzhillstudios.parse-utils
  (:gen-class)
  (:use [com.marzhillstudios.test.tap :only [test-tap is ok]]
     [com.marzhillstudios.util :only [defmulti-]]))

(defn- mk-leaf [nm]
  nm)

(defn grammar
  "Returns a function that will parse a sequence
   using the provided parse-tree."
  [parse-tree] )

(defn match-drop
  "Returns a function that matches/consumes the matchers but does not
   return them. The function will return nil if the match fails."
  [matchers] nil)

(defn forward-match
  "Returns a function that does a forward match but does not consume
   the matched tokens. The function will return nill if the match fails."
  [matchers] nil)

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
(defn list-match [& l]
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

(defn space []
  "Returns a function that matches/consumes exactly one space."
  (exact " "))

(defn- exact-token-maybe
  ([token s] (exact-token-maybe "" token s))
  ([acc token s]
   ( cond (empty? token) {:tree (mk-leaf acc) :rest s}
     (empty? s) nil
     :else (let [sc (first s)
                 tc (first token)]
             (cond
               (= sc tc) (recur (str acc sc) (drop 1 token) (drop 1 s))
               :else nil)))))
(defn exact
  "Returns a function that reads a token from a sequence.
   Returns token and rest of seq if matched, nil if no match."
  [token] (partial exact-token-maybe token))

(defn- until-token-maybe
  ([token s] (until-token-maybe "" token s))
  ([acc token s] (let [token? (exact-token-maybe token s)]
                   (cond (empty? s) nil
                     (nil? token?) (recur (str acc (first s))
                                          token (drop 1 s))
                     :else {:tree (mk-leaf [(mk-leaf (str acc))
                                              (:tree token?)])
                            :rest (:rest token?)}))))
(defn until 
  "Returns a function that reads up to a token from a sequence.
   Returns portion of the sequence that matched and the rest. nil if no match."
  [token] (partial until-token-maybe token))

(defn test-suite []
  (test-tap 12
            (is {:tree (mk-leaf "foo") :rest (seq " bar")}
                (exact-token-maybe "foo" "foo bar"))
            (is {:tree (mk-leaf [(mk-leaf "foo ")
                         (mk-leaf "bar")])
                 :rest ()}
                (until-token-maybe "bar" "foo bar"))
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
            ))

