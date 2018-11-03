(in-package #:erjoalgo-webutil)

(defstruct oauth-client
  client-id
  client-secret
  token-uri
  scopes
  auth-uri
  redirect-uris
  auth-provider-x-509-cert-url
  project-id
  key
  meta)

(defstruct oauth-token
  access-token
  refresh-token
  expires-in
  token-type
  error
  error-description
  scope
  obtained-at)

(defstruct api-login
  key
  token
  client)

(defun oauth-make-client-from-file (filename &key (json-path-to-client))
  (let* ((top-json
          (cl-json:decode-json-from-source (pathname filename)))
         (client-json
          (if json-path-to-client
              (-json-get-nested top-json json-path-to-client)
              top-json))
         (client (make-from-json-alist client-json oauth-client)))
    (assert top-json)
    (assert client-json)
    (with-slots (client-id client-secret) client
      (assert client-id)
      (unless client-secret
        (warn "missing client secret")))
    client))

(defun fetch-token (oauth-client redirect-uri)
  (with-slots (client-id client-secret token-uri) oauth-client
    (-> (drakma:http-request token-uri :parameters
                             (append
                              (params
                               "client_id" client-id
                               "redirect_uri" redirect-uri
                               "grant_type" "authorization_code")
                              (when client-secret
                                `(("client_secret" ,client-secret))))
                             :WANT-STREAM t)
        (cl-json:decode-json-from-source))))

(defun oauth-server-redirect-url (oauth-client local-redirect-uri scopes-to-request)
  "Example
   https://accounts.google.com/o/oauth2/v2/auth?
    scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fdrive.metadata.readonly&
    access-type=offline&
    include-granted-scopes=true&
    state=state-parameter-passthrough-value&
    redirect-uri=http%3A%2F%2Foauth2.example.com%2Fcallback&
    response-type=code&
    client-id=client-id"

  (with-slots (scopes client-id auth-uri) oauth-client
    (format nil "~A?~A"
            auth-uri
            ;; "http://localhost:1234/"
            (-> (params
                 "scope" (format nil "~{~A~^ ~}" scopes-to-request)
                 "access_type" "online"
                 "include_granted_scopes" "false"
                 "redirect_uri" local-redirect-uri
                 "response_type" "code"
                 "client_id" client-id)
                (drakma::alist-to-url-encoded-string :utf-8 'drakma:url-encode)))))


(defun oauth-token-auth-header (oauth-token)
  (cons :authorization
        (format nil "~A ~A"
                (or (oauth-token-token-type oauth-token) "bearer")
                (oauth-token-access-token oauth-token))))

(defun oauth-exchange-code-for-token (code oauth-client
                                      &key (redirect-uri
                                            (car (oauth-client-redirect-uris
                                                  oauth-client))))
  "code	The authorization code returned from the initial request.
   client_id	The client ID obtained from the API Console.
   client_secret	The client secret obtained from the API Console.
   redirect_uri	One of the redirect URIs listed for your project in the API Console.
   grant_type	As defined in the OAuth 2.0 specification,
   this field must contain a value of authorization_code."

  (assert code)
  (with-slots (scopes client-id client-secret token-uri redirect-uris) oauth-client
    (multiple-value-bind (content status)
        (let ((query-params (params
                             "code" code
                             "grant_type" "authorization_code"
                             "client_secret" client-secret
                             "redirect_uri" redirect-uri
                             "client_id" client-id)))
          (vom:debug "exchange-token: ~A query params: ~A~%"
                     token-uri query-params)
          (erjoalgo-webutil:api-req
           (make-http-request
            :method :post
            :qparams query-params)
           :api-base-url
           token-uri))
      ;; (vom:debug "headers: ~A~%" headers)
      (if (not (eq 200 status))
          (error "non-200 status code: ~A ~A" status content)
          (let ((json content))
            (->
             json
             (make-from-json-alist oauth-token)))))))

                                        ; TODO make this work with non-google oauth servers
(defun hunchentoot-create-oauth-redirect-handler
    (oauth-client scopes-to-request handler
     &key (oauth-authorize-uri-path "/oauth/authorize")
       (scheme "https")
       (login-session-key :login)
       (original-url-session-key 'original-url)
       on-authenticated-fn)

  ;; "Hunchentoot oauth middleware dispatcher:
  ;;  If auth is missing, redirect to remote authorization server.
  ;;  Otherwise, if the path matches OAUTH-AUTHORIZE-URI-PATH,
  ;;  exchange code for token and associate token with session.
  ;;  Otherwise, allow other dispatchers to handle the request."

  ;; the dispatcher returns a closure which is invoked with the request object
  ;; and returns a handler iff it can handle the request

  ;; A request handler is a function of zero arguments which relies on
  ;; the special variable *REQUEST* to access the request instance being serviced

  (lambda ()
    (symbol-macrolet ((login (hunchentoot:session-value login-session-key)))
      (let ((local-auth-url (format nil "~A://~A~A"
                                    scheme
                                    (hunchentoot:host)
                                    oauth-authorize-uri-path))
            (request hunchentoot:*request*))
        (log-request "oauth-middleware")

        ;; TODO handle this in another layer
        (unless hunchentoot:*session* (hunchentoot:start-session))
        (assert hunchentoot:*session*)

        ;; maybe invalidate session if token expired
        (when login
          (with-slots (expires-in obtained-at) (api-login-token login)
            (assert obtained-at)
            (let* ((now (GET-UNIVERSAL-TIME))
                   (expires-at (+ obtained-at expires-in)))
              (when (>= now expires-at)
                (vom:warn "token expired. invalidating session login credentials...")
                (setf login nil)))))

        (if login
            ;; non-nil login implies not expired and hopefully valid
            ;; TODO maybe invalidate on 401 handler retcode
            (funcall handler)

            ;; not authenticated
            (if (equal oauth-authorize-uri-path (hunchentoot:script-name request))
                ;; back from the authorization server... exchange code for token
                ;; (assert (hunchentoot:session-value original-url-session-key))
                (let* ((original-url
                        (check-nonnil (hunchentoot:session-value original-url-session-key)))
                       (code (-> (hunchentoot:get-parameters request)
                                 (assoq "code")
                                 check-nonnil))
                       (oauth-token (check-nonnil
                                     (oauth-exchange-code-for-token
                                      code oauth-client
                                      :redirect-uri local-auth-url))))

                  (vom:debug "back from auth server with params: ~A~%"
                             (hunchentoot:get-parameters request))

                  (if (oauth-token-access-token oauth-token)
                      ;; successfully obtained token
                      (progn
                        (setf
                         (slot-value oauth-token 'obtained-at) (GET-UNIVERSAL-TIME)
                         login (make-api-login
                                ;; TODO does this make sessions 'heavy'?
                                ;; may compromise client credentials?
                                :client oauth-client
                                :key nil
                                :token oauth-token))
                        (when on-authenticated-fn
                          (funcall on-authenticated-fn login))
                        (hunchentoot:redirect original-url))

                      ;; exchange for token was rejected
                      (progn (setf (hunchentoot:return-code*)
                                   hunchentoot:+http-authorization-required+)
                             ;; TODO return json
                             (vom:info "token request rejected: ~A~%" oauth-token)
                             (erjoalgo-webutil:json-resp
                              `((:error . "token request rejected")
                                ;; cl-json complains:
                                ;; (my-clos-object)
                                ;; is not of a type which can be encoded by ENCODE-JSON
                                ;; ...
                                (:details . ,(prin1-to-string oauth-token)))))))

                ;; redirect to remote auth server
                (let* ((remote-auth-url (oauth-server-redirect-url
                                         oauth-client
                                         local-auth-url
                                         scopes-to-request)))
                  (setf (hunchentoot:session-value original-url-session-key)
                        (hunchentoot:request-uri request))
                  (vom:debug "redirecting to remote oauth: ~A~%"
                             remote-auth-url)
                  (vom:debug "local redirect url: ~A~%"
                             local-auth-url)
                  (hunchentoot:redirect remote-auth-url))))))))
