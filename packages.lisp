(defpackage #:erjoalgo-webutil
  (:use :cl :cl-markup)
  (:export #:read-file
           #:retry-times
           #:assoq
           #:make-from-json-alist
           #:->
           #:with-json-paths
           #:lisp-to-json-key
           #:json-key-to-lisp
           #:params
           #:json-path-split
           #:json-get-nested
           #:-json-get-nested))

(defpackage #:erjoalgo-webutil/google
  (:use :cl #:erjoalgo-webutil)
  (:export #:make-oauth-client-from-file
           #:OAUTH-TOKEN-AUTH-HEADER
           #:AUTH-SERVER-REDIRECT-URL
           #:EXCHANGE-CODE-FOR-TOKEN
           #:RESP-TOKEN-ACCESS-TOKEN
           #:RESP-TOKEN-REFRESH-TOKEN
           #:make-api-login
           #:api-req
           #:defapi-endpoint
           #:defapi))
