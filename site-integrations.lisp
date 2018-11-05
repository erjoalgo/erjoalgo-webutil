(in-package #:erjoalgo-webutil)

(defvar sexchange-login-key :sexchange-login)

(defvar *sexchange-login* nil)

(defun sexchange-authenticator (http-request is-refresh-p
                                &key (login-key sexchange-login-key))
  (when is-refresh-p
    (error "not implemented"))
  (let* ((login (or *sexchange-login*
                    (hunchentoot:session-value login-key)))
         key)
    (with-slots (token client) login
      (setf key (oauth-client-key client))
      ;; authentication not always required by the api
      ;; (assert key)
      ;; (assert (or key token client))
      (with-slots (qparams) http-request
        (when key
          (push (cons "key" key) qparams))
        (when token
          (push (cons "access_token" (oauth-token-access-token token))
                qparams))))))

(defun sexchange-depaginator (resp-body http-request page-idx)
  (with-slots (qparams) http-request
    (if (null resp-body)
        (progn
          (assert (eq page-idx 1))
          (push (cons "page" "1") qparams)
          (values nil t))
        (with-json-paths resp-body
            ((items "items")
             (has-more "has_more"))
          (if has-more
              (let ((page-cons (assoc "page" qparams :test 'equal)))
                ;; increase page number
                (assert page-cons)
                (setf (cdr page-cons)
                      (write-to-string (1+ (parse-integer (cdr page-cons)))))
                (vom:debug "new qparams: ~A~%" qparams)
                (values items t))
              (values items nil))))))

(defvar google-login-key :google-login)

(defun google-depaginator (resp-body http-request page-idx)
  (with-slots (qparams) http-request
    (if (null resp-body)
        (progn
          (assert (eq page-idx 1))
          (values nil t))
        (with-json-paths resp-body
            ((items "items")
             (page-token "nextPageToken"))
          (if page-token
              (let ((page-cons
                     (or (assoc "pageToken"
                                qparams :test 'equal)
                         (car (push (cons "pageToken" 1) qparams)))))
                ;; increase page number
                ;; (assert page-cons)
                (setf (cdr page-cons) page-token)
                (vom:debug "new qparams: ~A~%" qparams)
                (values items t))
              (values items nil))))))

(defvar *google-login* nil)

(defun google-authenticator (http-request is-refresh-p
                             &key (login-key google-login-key))
  (declare (ignore is-refresh-p))
  (let* ((login (or *google-login*
                    (hunchentoot:session-value login-key))))
    (vom:debug "login is ~A~%" login)
    (with-slots (client token key) login
      (setf key (or key (when client (oauth-client-key client))))
      (format t "api-client: value of key: ~A~%" key)
      (format t "api-client: value of token: ~A~%" token)
      (assert (= 1 (+ (if key 1 0)
                      (if token 1 0)))
              (login)
              "must provide exactly one auth method: ~A" login)
      (with-slots (qparams additional-headers) http-request
        (if token
            (with-slots (access-token token-type) token
              (assert (and access-token token-type))
              (push (cons "authorization"
                          (format nil "~A ~A"
                                  (or token-type "bearer")
                                  access-token))
                    additional-headers)))
        (push (cons "key" key) qparams)))))

(defun google-userinfo-email ()
  (api-req
   (make-http-request
    :resource "/plus/v1/people/me")
   :api-base-url "https://www.googleapis.com"
   :authenticator 'erjoalgo-webutil:google-authenticator))

(defun google-current-session-user-email (&key (email-key :email))
  "Return the current authenticated user's email.
   If it does not exist, it is fetched and cached in the session."
  (or
   (hunchentoot:session-value email-key)
   (setf
    (hunchentoot:session-value email-key)
    (->
     (google-userinfo-email)
     (-json-get-nested "emails[0].value")))))
