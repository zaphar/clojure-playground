(ns
  com.marzhillstudios.parse-utils
  (:gen-class)
  (:use [com.marzhillstudios.test.tap :only [test-tap is ok]]))

(defn- any-of
  "Function that returns the first match of any of the provided matchers."
  ([matchers s]
   (let [candidate ((first matchers) s)]
     (cond (nil? candidate) (recur (drop 1 matchers) s)
       :else candidate))))
(defn any [& matchers] (fn [s] (any-of matchers s)))

(defn- repeated
  ([acc matcher s]
    (let [matched (matcher s)]
      (cond (nil? matched) {:token (:token acc) :rest s}
        :else (repeated {:token (conj (:token acc) (:token matched))
                          :rest ""}
                          matcher (:rest matched)))))
  ([matcher s]
    (let [matched (matcher s)]
      (cond (nil? matched) nil
        :else (repeated {:token [(:token matched)] :rest ""}
                          matcher (:rest matched))))))

(defn- exact-token-maybe
  "Function that reads a token from a sequence.
   Returns token and rest of seq if matched, nil if no match."
  ([token s] (exact-token-maybe "" token s))
  ([acc token s]
   ( cond (empty? token) {:token acc :rest s}
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
                     :else {:token (str acc (:token token?))
                            :rest (:rest token?)}))))
(defn until [token] (partial until-token-maybe token))

(defn test-suite []
  (test-tap 4
            (is {:token "foo" :rest (seq " bar")}
                (exact-token-maybe "foo" "foo bar"))
            (is {:token "foo bar" :rest ()}
                (until-token-maybe "bar" "foo bar"))
            (is {:token "foo" :rest (seq " bar")}
                (any-of [(exact "baz")
                         (until "bork")
                         (exact "foo")]
                        "foo bar"))
            (is {:token "foo bar" :rest ()}
                ((any (exact "baz")
                      (until "bar")
                      (exact "foo"))
                       "foo bar"))
            (is {:token ["foo" "foo"] :rest (seq " bar")}
                (repeated (exact "foo") "foofoo bar"))
            ))

