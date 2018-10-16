(in-package #:erjoalgo-webutil)

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
  (loop for (k . v) in params
     as k-string = (typecase k
                     (string k)
                     (symbol (lisp-to-json-key k))
                     (t (error "invalid alist key type: ~A" k)))
     unless (assoc k-string params :test #' equal)
     collect (cons k-string v) into params
     finally (return params)))

(defun api-req (login resource params-alist
                &key (method :get)
                  (depaginate-p t)
                  (retry-count 500)
                  (retry-delay 2)
                  (auto-refresh-p t))
  "retuns values: json-as-alist http-resp-code resp-string"
  (let* ((api-base-url default-api-base-url)
         (url (concatenate 'string api-base-url resource))
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
      (if (not depaginate-p)
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

(defmacro def-api-endpoint (resource-as-sym &key defaults
                                              (fun-sym resource-as-sym))
  `(defun ,fun-sym (login &rest params-flat)
     (let ((params-alist
           (loop for (k v) on params-flat by #'cddr
              collect (cons k v))))
       (api-req login ,(lisp-to-json-key resource-as-sym)
                (append params-alist ,defaults)))))
