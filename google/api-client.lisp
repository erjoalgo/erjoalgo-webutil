(in-package #:erjoalgo-webutil/google)

(export
 '(api-req
   defapi-endpoint
   defapi
   *api-base-url*))

(defstruct resp-page
  items
  page-info
  next-page-token
  etag
  kind
  error)

(defstruct api-login
  key
  token)

(defun alist-to-http-params (params)
  "Convert an lisp alist of key-value pairs PARAMS
   into drakma-compatible http-params."
  (loop for (k . v) in params
     as k-string = (typecase k
                     (string k)
                     (symbol (lisp-to-json-key k))
                     (t (error "invalid alist key type: ~A" k)))
     unless (assoc k-string params :test #' equal)
     collect (cons k-string v) into params
     finally (return params)))

(defvar *api-base-url* nil
  "default base url used by api-req")

(defun api-req (login resource params-alist
                &key (method :get)
                  (depaginate t)
                  (retry-count 500)
                  (retry-delay 2)
                  (auto-refresh-p t)
                  (api-base-url *api-base-url*))

  "Make a REST api request to the URL: API-BASE-URL/RESOURCE, with query parameters
   PARAMS-ALIST. Retry RETRY-COUNT times with a delay of RETRY-DELAY seconds
   after eeach retry in case of network errors.

   Returns values (DECODED-JSON HTTP-RESP-CODE RESP-STRING)

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

  (let* ((url (concatenate 'string api-base-url resource))
         (params (alist-to-http-params params-alist))
         additional-headers)

    (assert (= 1 (+
                  (if (api-login-token login) 1 0)
                  (if (api-login-key login) 1 0))))

    (if (api-login-key login)
        (push (cons :key (api-login-key login)) params)
        (push (oauth-token-auth-header (api-login-token login))
              additional-headers))

    (labels ((req (&optional already-refreshed-p)
               (multiple-value-bind (octets http-code)
                   (retry-times retry-count retry-delay
                     (loop
                          named annoying-NS-TRY-AGAIN-CONDITION-retry
                        for i from 0 do
                          (handler-case
                              (return-from annoying-NS-TRY-AGAIN-CONDITION-retry
                                (drakma:http-request url
                                                     :method method
                                                     :parameters params
                                                     :additional-headers additional-headers))
                            (USOCKET:NS-TRY-AGAIN-CONDITION
                                (ex)
                              (format nil "failed with ~A: ~A retrying ~D... ~%"
                                      'USOCKET:NS-TRY-AGAIN-CONDITION ex i)
                              (sleep 1)))))
                 (if (and auto-refresh-p (= 403 http-code) (not already-refreshed-p))
                     (progn (format t "got 403. trying to refresh..." )
                            (req t))
                     (let* ((string (babel:octets-to-string octets :encoding :utf-8))
                            (json (cl-json:decode-json-from-string string)))
                       (values json http-code string))))))
      (if (not depaginate)
          (req)
          (loop
             with page-token-param = (cons "pageToken" nil)
             with total-pages = -1
             for page-idx from 1

             as resp-string = nil
             as page = nil
             as error = nil
             as status-code = nil

             do (multiple-value-bind (body http-code string) (req)
                  (setf resp-string string
                        status-code http-code
                        ;; this may fail?
                        page (make-from-json-alist body resp-page)
                        error (resp-page-error page)))
             do (format t "page: ~A/~A params: ~A~%" page-idx
                        (ceiling total-pages)
                        params)
             when (and (null error)
                        ;2xx
                       (eq 2 (floor status-code 100)))
             do
               (progn
                 (setf (cdr page-token-param)
                       (resp-page-next-page-token page))

                 (when (eq -1 total-pages)
                   (with-json-paths (resp-page-page-info page)
                       ((per-page "resultsPerPage")
                        (total "totalResults"))
                     (setf total-pages (ceiling total (if (zerop per-page) -1 1))
                           params (cons page-token-param params)))))


             append (resp-page-items page) into items

             while (and (cdr page-token-param) (not error))

             finally (progn
                       (format t "fetched ~A items~%" (length items))
                       (return (values items status-code resp-string error))))))))

(defmacro defapi-endpoint (name method api-base-url
                           &rest rest
                           &key default-params
                             (resource-path (lisp-to-json-key name))
                             &allow-other-keys)
  ;; "Defines a function FUN-SYM that calls an api-endpoint.
  ;;  DEFAULT-PARAMS specifies defaults parameters used in the request
  ;;  unless overridden later by the caller."
  `(defun ,(intern (format nil "~A-~A" name method)) (login &rest params-flat)
     ,(format nil "~A ~A/~A ~A" method api-base-url resource-path
              (or default-params ""))
     (api-req login ,resource-path
              (append (params params-flat) ,default-params)
              :api-base-url ,api-base-url
              ,@(loop for (k v . etc) on rest by #'cddr
                   unless (member k '(:default-params
                                      :name
                                      :resource-path
                                      :api-base-url))
                   append (list k v)))))

(defmacro defapi (base-url &key
                             get
                             get-depaginate
                             post
                             delete)
  `(let ()
     ;; (*api-base-url* ,base-url)
     ,@(loop for (method kw-args endpoints)
          in
            `((:get ,get)
              (:get (:depaginate t) ,get-depaginate)
              (:post ,post)
              (:delete ,delete))
          append (loop for (name . endpoint-params) in endpoints
                    collect `(defapi-endpoint ,name ,method
                               ,base-url
                               ,@(append endpoint-params kw-args))))))
