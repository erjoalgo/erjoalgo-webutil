(in-package #:erjoalgo-webutil)

(defstruct resp-page
  items
  page-info
  next-page-token
  prev-page-token ;;not used, but avoids json unmarshal warning
  etag
  kind
  error)

(defun alist-to-http-params (params)
  "Convert an lisp alist of key-value pairs PARAMS
   into drakma-compatible http-params.
   Later parameters override earlier ones
   to support multiple levels of customization."
  (loop for (k . v) in (reverse params)
     as k-string = (typecase k
                     (string k)
                     (symbol (lisp-to-json-key k))
                     (t (error "invalid alist key type: ~A" k)))
     as v-string = (typecase v
                   (string v)
                   (symbol (lisp-to-json-key v))
                   (number (write-to-string v))
                   (t (error "invalid alist key type: ~A" v)))
     unless (assoc k-string new-params :test #' equal)
     collect (cons k-string v-string) into new-params
     finally (return new-params)))

(defvar *api-base-url* nil
  "default base url used by api-req")

(defvar *api-req-retry-count* 3
  "default retry count used by api-req")

(defstruct http-request
  method
  ;; base-url
  resource
  qparams
  additional-headers
  content
  rest)

(defun api-req (http-request
                &key
                  authenticator
                  depaginator
                  max-pages
                  token-refresh-p
                  (retry-count *api-req-retry-count*)
                  (retry-delay 2)
                  (api-base-url *api-base-url*)
                  content-type-override)

  "Make a REST api request to the URL: (CONCAT API-BASE-URL RESOURCE)
   with query parameters PARAMS-ALIST. (Note that no slash is added in between.)
   Retry RETRY-COUNT times with a delay of RETRY-DELAY seconds
   after each retry in case of network errors.

   Returns values (DECODED-JSON RESP-CODE RESP-STRING)

   - DECODED-JSON is a json object decoded via cl-json
   - HTTP-RESP-CODE is the http response code as a number
   - RESP-STRING is the raw response string before cl-json decoding

   If DEPAGINATE is non-nil, attempt to depaginate the requested resource
   by making several requests. If DEPAGINATE is a positive number, it specifies the max
   number of pages to request, otherwise no such limit is imposed.

   When depagination is on:

   - DECODED-JSON object is a list of resources across all requesed pages.
   - RETRY-COUNT applies to each page request.
   - RESP-STRING, HTTP-RESP-CODE values are based on the last retrieved page.
   - A fourth value ERROR is returned. Any non-2xx response code or persistent
     network error short-circuits depagination: partial results are returned
     and details of the error are stored in ERROR

   When AUTO-REFRESH-P is non-nil, an one-time attempt (per retry) is made, if possible,
   to refresh the token on 403 errors."

  (with-slots (method resource qparams additional-headers content rest) http-request
    (unless method
      (setf method (if content :post :get)))
    (unless (or (eq method :get)
                (assoc :content-type additional-headers))
      (push (cons :content-type (if qparams "application/x-www-form-urlencoded"
                                    "application/json")) additional-headers))
    (when authenticator
      ;; mutate http-request to add auth info
      (funcall authenticator http-request nil))
  (let* ((url (concatenate 'string api-base-url resource)))

    (labels ((req (&optional already-refreshed-p)
                 (vom:debug "~A ~A ~A~%" method url qparams)
                 (vom:debug "http-request ~A~%" http-request)
                 (vom:debug "depaginator: ~A~%" depaginator)

                 (multiple-value-bind (content http-code resp-headers)
                   (loop
                      named annoying-NS-TRY-AGAIN-CONDITION-retry
                      with _ex = nil
                      for i below RETRY-COUNT do
                        (handler-case
                            (return-from annoying-NS-TRY-AGAIN-CONDITION-retry
                              (apply
                               'drakma:http-request url
                               :method method
                               ;; TODO compute this once. force enhancers to produce string
                               :parameters (alist-to-http-params qparams)
                               :content content
                               :additional-headers additional-headers
                               rest))
                          ((or USOCKET:NS-TRY-AGAIN-CONDITION USOCKET:TIMEOUT-ERROR)
                              (ex)
                            (setf  _ex ex)
                            (vom:debug "~A failed with ~A: ~A retrying ~D... ~%"
                                       (format nil "~{~A~^ ~}"
                                                 (list url method qparams))
                                       'USOCKET:NS-TRY-AGAIN-CONDITION ex i)
                            (sleep retry-delay)))
                        finally (progn
                                  (vom:warn "exhaused retries. signalling ~A~%" _ex)
                                  (signal _ex)))
                   (assert http-code)
                   (if (and (= 403 http-code) token-refresh-p (not already-refreshed-p))
                     (progn (vom:warn "got 403. trying to refresh..." )
                                        ; TODO refresh token here
                              ;; mutate http-request to refresh the token
                              (funcall authenticator http-request t)
                            (req t))
                       (let ((usable-content content))
                         (with-json-paths resp-headers
                             ((content-encoding "content-encoding")
                              (content-type "content-type"))
                           (when content-type-override
                             (setf content-type content-type-override))
                           (if (equal "gzip" content-encoding)
                               (setf usable-content
                                     (gzip-stream::gunzip-byte-array usable-content)))

                           (vom:debug "content-type: ~A~%" content-type)
                           (cond
                             ((ppcre:scan "application/json" content-type)
                                        ; TODO get charset from encoding
                               (setf usable-content
                                     (-> usable-content
                                         (babel:octets-to-string :encoding :utf-8)
                                         (cl-json:decode-json-from-string))))
                             ((ppcre:scan "application/x-www-form-urlencoded" content-type)
                              (setf usable-content (drakma::dissect-query usable-content))))
                           (vom:debug "response: ~A~%" usable-content)
                           ;; (assert nil)
                           (values usable-content http-code content)))))))
        ;; (setf max-pages 30)
        (if (null depaginator)
          (req)
            (let (all-items last-http-code last-resp-body)
          (loop
             for page-idx from 1
                 as continue = (when (or (null last-http-code)
                                         (eq 2 (floor last-http-code 100)))
                                 (multiple-value-bind (items continue?)
                                     (funcall depaginator last-resp-body http-request page-idx)
                                   (when continue?
                                     ;; (assert (assoq (http-request-qparams http-request) "page"))
                                     )
                                        ; TODO destructive append
                                   (setf all-items (append all-items items))
                                   continue?))
                 while (and continue (or (null max-pages)
                                         (<= page-idx max-pages)))
             do
                   (multiple-value-bind (body http-code) (req)
                     (setf last-http-code http-code
                           last-resp-body body)
                     (vom:debug "page: ~A params: ~A~%"
                                page-idx
                                qparams))
                 finally
               (progn
                     (vom:debug "fetched ~A items~%" (length all-items))
                     (assert last-http-code)
                     (return (values all-items last-http-code
                                     last-resp-body))))))))))
