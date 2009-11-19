; Copyright (c) Jeremy Wall. All rights reserved.
; This software is available under the terms of the 
; Artistic License 2.0 
;   (http://www.opensource.org/licenses/artistic-license-2.0.php)
; By using this software in any fashion you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
(ns
  #^{:doc "TAP - Test Anything Protocol Testing framework",
     :author "Jeremy Wall <jeremy@marzhilltudios.com>",
     :license "Artistic License 2.0"
     :version "0.01.0"}
  test.tap
  (:gen-class)
  (:use dispatch.util))

(defn
  #^{:doc "tap diagnostic output function"}
  diag [msg]
  (println (format "# %s" msg)))

(defn
  #^{:doc "tap plan output function"}
  mk-plan [plan]
  (println (format "1..%s" plan)))

(defn
  #^{:doc "tap output function"}
  mk-tap [f msg test-count]
    ; TODO(jwall): handle exceptions correctly
    (try
      (if (f) (println (format "%s - ok %s" test-count msg))
        (println (format "%s - not ok %s" test-count msg)))
      (catch Exception e
        (println (format "%s - not ok %s died" test-count msg)))))

(defmacro
  run-tap-tests 
    ([test-num t & next]
      `(doall
         (run-tap-tests ~test-num ~t)
         (run-tap-tests (inc ~test-num) ~@next)))
    ([test-num t]
      `(mk-tap #(do ~t) '~t ~test-num))) 

; TODO(jwall): no-plan support
(defmacro
  #^{:doc "basic test runner to output assertions in tap format"}
   test-tap [plan & tests]
        `(doall (mk-plan ~plan)
          (run-tap-tests 1 ~@tests)))

;; helper test functions
(defn
  #^{:doc "ok - returns boolean true if logical true"}
  ok [b]
  (if b true false))

(defn
  #^{:doc "not-ok - returns boolean false if logical true"}
  not-ok [b]
  (if b false true))

(defn- diag-true [msg] 
  (diag msg)
  true)

(defn- diag-false [msg] 
  (diag msg)
  false)

(defn
  #^{:doc "is - returns true for equality false otherwise"}
  is [expected got]
  (cond
    (= expected got) true
    :else (diag-false (str "expected: " \[ expected \] " got: " \[ got \] ))))

(defmulti
  #^{:doc "is-deeply - returns true for equality. Does deep field inspection"}
  is-deeply maybe)
(defmethod is-deeply nil
  [] false)

;; sample test functions for testing the framework and reflection
(defn tap-test-fun []
  {:doc "sample testing function to test the tap function"}
  (diag "running tap-test-fun")
  true)

(defn tap-test-fun-exception []
  {:doc "sample testing function to test the tap function"}
  (diag "running tap-test-fun")
  (throw (Exception. "I'm dying! Auuggghhhh......")))

(defn test-set-to-run []
  {:doc "sample set of tests"}
  (test-tap 4
    (= 1 1)
    (not (= 1 2))
    (tap-test-fun-exception)
    (tap-test-fun)))

; helpful reflection functions to examine the internals of
; the test-tap functions
(defn reflect-test-tap []
  (macroexpand `(test-tap 2
                          (= 1 1)
                          (not (= 1 1)))))

