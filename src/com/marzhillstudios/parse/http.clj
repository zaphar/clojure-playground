(ns
  com.marzhillstudios.parse.http
  (:gen-class)
  (:use [com.marzhillstudios.test.tap :only [test-tap is ok]]
        [com.marzhillstudios.parse-utils]))

(def terminator (any (crlf) (lf)))

(def http-method (annotated :method (until (ignore (repeated (space))))))

(def http-local-resource (annotated :resource (until (ignore (repeated (space))))))

(def http-version (annotated :http-version
                             (re-match #" *HTTP/([0-9]\.[0-9]) *")))

(def http-status-code (annotated :status-code
                                 (re-match #" *([0-9][0-9][0-9]) *")))

(def http-status-message (annotated :status-message
                                    (until (ignore terminator))))

(def http-request-line (annotated :request-line
                                  (list-match
                                    (merge-annotations
                                      http-method
                                      http-local-resource
                                      http-version)
                                    (ignore (re-match #" *(\r\n|\n)")))))

(def http-status-line
  (annotated :status-line
             (merge-annotations
               http-version
               http-status-code
               http-status-message)))

(def http-initial-line (any http-status-line http-request-line))

(def http-header-key
  (annotated :key
             (re-match #"^([^:\r\n]+):")))
  
(def http-header-value
    (annotated :value
               (any
                 (until (forward-match http-header-key))
                 (until (ignore terminator)))))

(def http-header-line
  (merge-annotations
    http-header-key
    http-header-value))

(def http-headers
  (annotated :http-headers (repeated http-header-line)))

(def http-grammar
  (list-match http-initial-line http-headers))

(defn parse-http [s]
  (apply-grammar http-grammar s))

