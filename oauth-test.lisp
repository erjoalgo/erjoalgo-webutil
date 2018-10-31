(fiasco:define-test-package #:erjoalgo-webutil/oauth/test
  (:use #:erjoalgo-webutil
        #:erjoalgo-webutil))

(in-package #:erjoalgo-webutil/oauth/test)

(defparameter test-client
  (erjoalgo-webutil::make-oauth-client
   :CLIENT-ID "***REMOVED***"
   :CLIENT-SECRET "***REMOVED***"
   :TOKEN-URI "https://stackoverflow.com/oauth/access_token"
   :SCOPES "write_access,private_info"
   :AUTH-URI "https://stackoverflow.com/oauth"
   :REDIRECT-URIS NIL
   :AUTH-PROVIDER-X-509-CERT-URL NIL
   :PROJECT-ID NIL))

(deftest test-resp-parse ()
  (let ((token
         (->
          "access_token=***REMOVED***&expires=86400"
          (drakma::dissect-query)
          (erjoalgo-webutil:make-from-json-alist
           erjoalgo-webutil::oauth-token))))
    (is (equal (erjoalgo-webutil::oauth-token-access-token
                token)
               "***REMOVED***"))))

(deftest test-exchange-code-for-token ()
  (erjoalgo-webutil::with-mock
      (drakma:http-request (lambda (&rest args)
                             (vom:debug "args: ~A~%" args)
                             (values
                              "access_token=***REMOVED***&expires=86400"
                              200
                              `((:content-type . "application/x-www-form-urlencoded")))))
    (let ((token
           (oauth-exchange-code-for-token "123" test-client)))
      (is (equal (slot-value token 'erjoalgo-webutil::access-token)
                 "***REMOVED***")))))

(defparameter sample-client-json
    "{
    \"client-id\": \"***REMOVED***\",
    \"client-secret\": \"***REMOVED***\",
    \"key\": \"***REMOVED***\",
    \"token-uri\": \"https://stackoverflow.com/oauth/access_token\",
    \"auth-uri\": \"https://stackoverflow.com/oauth\",
    \"scopes\": \"write_access,private_info\"
}")

(deftest test-read-client-from-file ()
  (let ((tmp-pathname #P"/tmp/oauth-test.json"))
    (with-open-file (fh tmp-pathname
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
      (format fh "~A" sample-client-json))
    (with-slots
          (erjoalgo-webutil::client-id
           erjoalgo-webutil::client-secret
           erjoalgo-webutil::key
           erjoalgo-webutil::token-uri)
        (erjoalgo-webutil:oauth-make-client-from-file
         tmp-pathname)
      (is (equal erjoalgo-webutil::client-id "***REMOVED***"))
      (is (equal erjoalgo-webutil::client-secret "***REMOVED***"))
      (is (equal erjoalgo-webutil::key "***REMOVED***"))
      (is (equal erjoalgo-webutil::token-uri "https://stackoverflow.com/oauth/access_token")))))

(run-package-tests :interactive t)
