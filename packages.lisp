(defpackage #:erjoalgo-webutil
  (:use :cl)
  (:export

   ;; util
   #:read-file
   #:assoq
   #:->
   #:->>

   ;; json
   #:make-from-json-alist
   #:with-json-paths
   #:json-get-nested
   #:-json-get-nested
   #:json-key-to-lisp
   #:lisp-to-json-key
   #:json-path-split
   #:json-resp
   #:json-req

   ;; http
   #:retry-times
   #:drakma-json-content-type-hack
   #:params
   #:-params
   #:first-file-with-extension
   #:check-nonnil
   #:log-request))

(defpackage #:erjoalgo-webutil/google
  (:use :cl :erjoalgo-webutil)
  (:export
   #:api-req
   #:defapi-endpoint
   #:defapi
   #:*api-base-url*

   #:MAKE-OAUTH-CLIENT-FROM-FILE
   #:OAUTH-TOKEN-AUTH-HEADER
   #:AUTH-SERVER-REDIRECT-URL
   #:EXCHANGE-CODE-FOR-TOKEN
   #:RESP-TOKEN-ACCESS-TOKEN
   #:RESP-TOKEN-REFRESH-TOKEN
   #:make-api-login
   #:CREATE-HUNCHENTOOT-OAUTH-REDIRECT-DISPATCHER
   #:DEFINE-REGEXP-ROUTE
   #:session-user-email
   #:google-userinfo-email))
