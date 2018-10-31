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
   #:defroutes
   #:retry-times
   #:drakma-json-content-type-hack
   #:params
   #:-params
   #:first-file-with-extension
   #:check-nonnil
   #:make-directories-recursively
   #:log-request
   #:with-gensyms
   #:slot-value->
   #:hunchentoot-make-add-fake-session

   #:api-req
   #:defapi-endpoint
   #:defapi
   #:*api-base-url*
   #:*api-req-retry-count*

   ;; oauth

   #:oauth-make-client-from-file
   #:oauth-token-auth-header
   #:oauth-server-redirect-url
   #:oauth-exchange-code-for-token
   #:oauth-token-access-token
   #:oauth-token-refresh-token
   #:make-api-login
   #:create-hunchentoot-oauth-redirect-dispatcher
   #:oauth-session-user-email
   #:google-userinfo-email
   #:http-request
   #:http-request-qparams
   #:make-http-request
   #:qparams
   #:soverflow-depaginator
   #:google-authenticator
   #:google-depaginator
   #:defendpoint
   #:*google-login*))
