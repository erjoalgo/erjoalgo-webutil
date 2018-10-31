(fiasco:define-test-package #:erjoalgo-webutil/google/api-client/test
  (:use #:erjoalgo-webutil/google
        #:erjoalgo-webutil))

(in-package #:erjoalgo-webutil/google/api-client/test)

(deftest test-api-client-depaginate ()
  (multiple-value-bind (body status)
      (api-req
       (make-http-request
        :resource "/answers"
        :qparams `(
                   ;; (:key . "1234")
                   ;; (:access_token . "4321")
                   ("site" . "stackoverflow")))
       :depaginator 'erjoalgo-webutil/google::sover-depaginator
       :max-pages 3)
    (vom:debug "body: ~A~%" body)
    (vom:debug "(length body): ~A~%" (length body))
    (is (eql status 200))))

(deftest test-api-client ()
  (multiple-value-bind (body status)
      (api-req
       (make-http-request
        :resource "/me/comments"
        :qparams `(
                   ;; (:key . "1234")
                   ;; (:access_token . "4321")
                   ("site" . "stackoverflow"))))
    (vom:debug "body: ~A~%" body)
    (is (eql status 400))))

(deftest test-api-client-www-encoded-resp ()
  (erjoalgo-webutil::with-mock
      (drakma:http-request (lambda (&rest args)
                             (declare (ignore args))
                             (vom:debug "calling mock http-reqest...")
                             (values
                              "access_token=***REMOVED***&expires=86400"
                              200
                              `((:content-type . "application/x-www-form-urlencoded")))))

    (is (equal "access_token=***REMOVED***&expires=86400"
               (drakma:http-request)))
    (multiple-value-bind (body)
        ;; TODO this looks like make-http-request is drakma:http-request
        (api-req (make-http-request :resource "/some-resource")
                 :api-base-url "http://example.com")
      (with-json-paths body
          ((token "access_token")
           (expires  "expires"))
        (is (equal token "***REMOVED***"))
        (is (equal expires 86400))))))

(let ((erjoalgo-webutil/google:*api-base-url*
       "https://api.stackexchange.com"))
  (run-package-tests :interactive t))
