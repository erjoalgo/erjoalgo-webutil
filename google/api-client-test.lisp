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

(let ((erjoalgo-webutil/google:*api-base-url*
       "https://api.stackexchange.com"))
  (run-package-tests :interactive t))
