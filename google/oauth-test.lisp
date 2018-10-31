(fiasco:define-test-package #:erjoalgo-webutil/google/oauth/test
  (:use #:erjoalgo-webutil/google
        #:erjoalgo-webutil))

(in-package #:erjoalgo-webutil/google/oauth/test)

(defparameter test-client
  (erjoalgo-webutil/google::make-oauth-client
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
           erjoalgo-webutil/google::oauth-token))))
    (is (equal (erjoalgo-webutil/google::oauth-token-access-token
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
      (is (equal (slot-value token 'erjoalgo-webutil/google::access-token)
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
          (erjoalgo-webutil/google::client-id
           erjoalgo-webutil/google::client-secret
           erjoalgo-webutil/google::key
           erjoalgo-webutil/google::token-uri)
        (erjoalgo-webutil/google:oauth-make-client-from-file
         tmp-pathname)
      (is (equal erjoalgo-webutil/google::client-id "***REMOVED***"))
      (is (equal erjoalgo-webutil/google::client-secret "***REMOVED***"))
      (is (equal erjoalgo-webutil/google::key "***REMOVED***"))
      (is (equal erjoalgo-webutil/google::token-uri "https://stackoverflow.com/oauth/access_token")))))

(run-package-tests :interactive t)
