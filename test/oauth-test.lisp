(fiasco:define-test-package
    #:erjoalgo-webutil/test
  (:use #:erjoalgo-webutil))

(in-package #:erjoalgo-webutil/test)

;; delete previously defined, renamed tests

;; (fiasco-cle"ar-tests)

(defvar json-foo_bar
  "{\"foo_bar\": 1}")

(defun object-bound-slots (obj)
  (remove-if-not (lambda (slot) (slot-boundp obj slot))
                 (mapcar (lambda (slot)
                           (slot-value slot 'sb-pcl::name))
                         (sb-mop:class-slots (class-of obj)))))

(defparameter client-json
  "{
  \"web\": {
    \"client_id\": \"REMOVED.apps.googleusercontent.com\",
    \"project_id\": \"the-id\",
    \"auth_uri\": \"https://accounts.google.com/o/oauth2/auth\",
    \"token_uri\": \"https://accounts.google.com/o/oauth2/token\",
    \"auth_provider_x509_cert_url\": \"https://www.googleapis.com/oauth2/v1/certs\",
    \"client_secret\": \"the-secret\",
    \"redirect_uris\": [
      \"***REMOVED***\",
      \"http://localhost\"
    ]
  }
}")


(deftest test-json-parsing ()
  (let ((json client-json)
        (tmp-filename #P"/tmp/oauth-test.json"))
    (with-open-file (fh tmp-filename
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
      (format fh "~A" json))

    (let ((oauth-client (make-oauth-client-from-file
                         tmp-filename)))
      (with-slots (client-secret
                   client-id
                   auth-uri)
          oauth-client
        (is (equal "the-secret" client-secret))
        (is (equal "REMOVED.apps.googleusercontent.com" client-id))))))

'(deftest test-exchange-code-for-token ()
  (flet ((drakma:http-request (&rest args)
           'caca))
    (is (eq (drakma:http-request) 'caca))
    '(exchange-code-for-token 1 2)))

'"
The following snippet shows a sample request:

POST /oauth2/v4/token HTTP/1.1
Host: www.googleapis.com
Content-Type: application/x-www-form-urlencoded

code=4/P7q7W91a-oMsCeLvIaQm6bTrgtp7&
client_id=your_client_id&
client_secret=your_client_secret&
redirect_uri=https://oauth2.example.com/code&
grant_type=authorization_code"


  '"
(response)
{
\"access_token\":\"1/fFAGRNJru1FTz70BzhT3Zg\",
\"expires_in\":3920,
\"token_type\":\"Bearer\",
\"refresh_token\":\"1/xEoDL4iW3cxlI7yDbSRFYNG01kVKM2C-259HOF2aQbI\"
}

"
'(progn
  (let ((client (make-oauth-client-from-file "py/client_secrets.json")))
    (setf resp (fetch-token client "localhost:4242"))))

"POST /oauth2/v4/token HTTP/1.1
Host: www.googleapis.com
Content-Type: application/x-www-form-urlencoded

code=4/P7q7W91a-oMsCeLvIaQm6bTrgtp7&
client_id=your_client_id&
client_secret=your_client_secret&
redirect_uri=https://oauth2.example.com/code&
grant_type=authorization_code
"

(run-package-tests :interactive t)
