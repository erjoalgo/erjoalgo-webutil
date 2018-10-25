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
   #:make-directories-recursively
   #:log-request))

(defpackage #:erjoalgo-webutil/google
  (:use :cl :erjoalgo-webutil)
  (:export
   #:api-req
   #:defapi-endpoint
   #:defapi
   #:*api-base-url*

   ;; oauth

   #:oauth-make-client-from-file
   #:oauth-token-auth-header
   #:oauth-server-redirect-url
   #:oauth-exchange-code-for-token
   #:oauth-token-access-token
   #:oauth-token-refresh-token
   #:make-api-login
   #:CREATE-HUNCHENTOOT-OAUTH-REDIRECT-DISPATCHER
   #:DEFINE-REGEXP-ROUTE
   #:oauth-session-user-email
   #:google-userinfo-email
   #:*api-req-retry-count*))
