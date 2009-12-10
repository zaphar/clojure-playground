(ns
  com.marzhillstudios.parse-utils
  (:gen-class)
  (:use [com.marzhillstudios.test.tap :only [test-tap is ok]]))

(defstruct token :name :children)
(defn- mk-token [nm ch]
  (struct-map token :name nm :children ch))

(defn- complex-match [match-spec s] nil)

(defn- any-of
  "Function that returns the first match of any of the provided matchers."
  ([matchers s]
   (let [candidate ((first matchers) s)]
     (cond (nil? candidate) (recur (drop 1 matchers) s)
       :else candidate))))
(defn any [& matchers] (fn [s] (any-of matchers s)))

(defn- repeated-match-n
  ([acc matcher n s]
    (let [matched (matcher s)
          cnt (inc (:count acc))]
      (cond
        (nil? matched) {:token (mk-token (:token acc) nil) :rest s}
        (= cnt n) {:token (mk-token (conj (:token acc)
                                          (:token matched)) nil)
                   :rest (:rest matched)}
        :else (recur {:token (conj (:token acc) (:token matched))
                          :rest ""}
                          matcher n (:rest matched)))))
  ([matcher n s]
    (let [matched (matcher s)]
      (cond (nil? matched) nil
        :else (repeated-match-n {:token [(:token matched)] :rest "" :count 1}
                          matcher n (:rest matched))))))
(defn repeated-n [matcher n] (fn [s] (repeated-match-n matcher n s)))

(defn- repeated-match
  ([acc matcher s]
    (let [matched (matcher s)]
      (cond (nil? matched) {:token (mk-token (:token acc) nil) :rest s}
        :else (recur {:token (conj
                               (:token acc)
                               (:token matched))
                      :rest ""}
                     matcher (:rest matched)))))
  ([matcher s]
    (let [matched (matcher s)]
      (cond (nil? matched) nil
        :else (repeated-match {:token [(:token matched)] :rest ""}
                          matcher (:rest matched))))))
(defn repeated [matcher]
    (fn [s] (repeated-match matcher s)))

(defn space [s]
  (exact " "))

(defn- exact-token-maybe
  "Function that reads a token from a sequence.
   Returns token and rest of seq if matched, nil if no match."
  ([token s] (exact-token-maybe "" token s))
  ([acc token s]
   ( cond (empty? token) {:token (mk-token acc nil) :rest s}
     (empty? s) nil
     :else (let [sc (first s)
                 tc (first token)]
             (cond
               (= sc tc) (recur (str acc sc) (drop 1 token) (drop 1 s))
               :else nil)))))
(defn exact [token] (partial exact-token-maybe token))

(defn- until-token-maybe
  "Function that reads up to a token from a sequence.
   Returns portion of the sequence that matched. nil if no match."
  ([token s] (until-token-maybe "" token s))
  ([acc token s] (let [token? (exact-token-maybe token s)]
                   (cond (empty? s) nil
                     (nil? token?) (recur (str acc (first s))
                                          token (drop 1 s))
                     :else {:token (mk-token [(mk-token (str acc) nil)
                                              (:token token?)] nil)
                            :rest (:rest token?)}))))
(defn until [token] (partial until-token-maybe token))

(defn test-suite []
  (test-tap 11
            (is {:token (mk-token "foo" nil) :rest (seq " bar")}
                (exact-token-maybe "foo" "foo bar"))
            (is {:token (mk-token [(mk-token "foo " nil)
                         (mk-token "bar" nil)] nil)
                 :rest ()}
                (until-token-maybe "bar" "foo bar"))
            (is {:token (mk-token "foo" nil)  :rest (seq " bar")}
                (any-of [(exact "baz")
                         (until "bork")
                         (exact "foo")]
                        "foo bar"))
            (is {:token (mk-token [(mk-token "foo " nil)
                         (mk-token "bar" nil)] nil)
                 :rest ()}
                ((any (exact "baz")
                      (until "bar")
                      (exact "foo"))
                       "foo bar"))
            (is {:token (mk-token [(mk-token "foo" nil)
                                   (mk-token "foo" nil)] nil)
                 :rest (seq " bar")}
                (repeated-match (exact "foo") "foofoo bar"))
            (is {:token (mk-token [(mk-token "foo" nil)
                                   (mk-token "foo" nil)] nil)
                 :rest (seq " bar")}
                ((repeated (exact "foo")) "foofoo bar"))
            (is {:token (mk-token " " nil) :rest (seq "bar")}
                ((space "bar") " bar"))
            (is {:token (mk-token [(mk-token "foo" nil)
                                   (mk-token "foo" nil)] nil)
                 :rest ()}
                ((repeated-n (exact "foo") 2) "foofoo"))
            ; not sure this is the correct behaviour?
            (is {:token (mk-token [(mk-token "foo" nil)] nil) :rest ()}
                ((repeated-n (exact "foo") 2) "foo"))
            (is {:token (mk-token [(mk-token "foo" nil)
                                   (mk-token "foo" nil)] nil)
                 :rest (seq " bar")}
                ((repeated-n (exact "foo") 2) "foofoo bar"))
            (is {:token (mk-token [(mk-token "foo" nil)
                                   (mk-token "foo" nil)] nil)
                 :rest (seq "foo bar")}
                ((repeated-n (exact "foo") 2) "foofoofoo bar"))
            ))

