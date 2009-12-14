(ns
  com.marzhillstudios.parse.http
  (:gen-class)
  (:use [com.marzhillstudios.test.tap :only [test-tap is ok]]
        [com.marzhillstudios.parse-utils]))

(def http-method (annotated :method (until (ignore (repeated (space))))))

(def http-local-resource (annotated :resource (until (ignore (repeated (space))))))

(def http-version (annotated :http-version
                             (re-match #" *HTTP/([0-9]\.[0-9]) *")))

(def http-status-code (annotated :status-code
                                 (re-match #" *([0-9][0-9][0-9]) *")))

(def http-status-message (annotated :status-message
                                    (until (ignore (crlf)))))

(def http-request-line (annotated :request-line
                                  (list-match
                                    http-method
                                    http-local-resource
                                    http-version
                                    (ignore (re-match #" *\r\n")))))

(def http-status-line
  (annotated :status-line
             (list-match
               http-version
               http-status-code
               http-status-message)))

(def http-initial-line (any http-status-line http-request-line))

(def http-grammar
  (list-match http-initial-line))

(defn parse-http [s]
  (apply-grammar http-grammar s))

