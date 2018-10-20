(in-package #:erjoalgo-webutil/google)

(defstruct oauth-client
  client-id
  client-secret
  token-uri
  scopes
  auth-uri
  redirect-uris
  AUTH-PROVIDER-X-509-CERT-URL
  PROJECT-ID)

(defun make-oauth-client-from-file (filename)
  (let* ((top-json
          (cl-json:decode-json-from-source filename))
         (client-json
          (-json-get-nested top-json "web"))
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

(defun auth-server-redirect-url (oauth-client local-redirect-uri scopes-to-request)
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

(defstruct resp-token
  access-token
  refresh-token
  expires-in
  token-type
  error
  error-description
  scope)

(defun oauth-token-auth-header (resp-token)
  (cons :authorization
        (format nil "~A ~A"
                (resp-token-token-type resp-token)
                (resp-token-access-token resp-token))))

(defun exchange-code-for-token (code oauth-client)
  "code	The authorization code returned from the initial request.
   client_id	The client ID obtained from the API Console.
   client_secret	The client secret obtained from the API Console.
   redirect_uri	One of the redirect URIs listed for your project in the API Console.
   grant_type	As defined in the OAuth 2.0 specification,
   this field must contain a value of authorization_code."

  (assert code)
  (with-slots (scopes client-id client-secret token-uri redirect-uris) oauth-client
    (->
     (drakma:http-request
      token-uri
      ;; "http://localhost:1234"
      :method :post
      :parameters (params
                   "code" code
                   "grant_type" "authorization_code"
                   "client_secret" client-secret
                   "redirect_uri" (car redirect-uris)
                   "client_id" client-id)
      :want-stream t)
     cl-json:decode-json-from-source
     (make-from-json-alist resp-token))))


(defun create-hunchentoot-oauth-redirect-dispatcher
    (oauth-client scopes-to-request
     &key (oauth-authorize-uri-path "/oauth/authorize")
       (scheme "https")
       (login-session-key :login)
       (original-url-session-key 'original-url)
       on-authenticated-fn)
  "Hunchentoot oauth middleware dispatcher:
   If auth is missing, redirect to remote authorization server.
   Otherwise, if the path matches OAUTH-AUTHORIZE-URI-PATH,
   exchange code for token and associate token with session.
   Otherwise, allow other dispatchers to handle the request."
  ;; the dispatcher returns a closure which is invoked with the request object
  ;; and returns a handler iff it can handle the request
  (lambda (request)
    (labels ((authenticated? ()
               (and hunchentoot:*session*
                    (hunchentoot:session-value login-session-key))))
      (vom:debug "oauth: value of (authenticated?): ~A~%" (authenticated?))
      (unless (authenticated?)
        (cond
          ((equal oauth-authorize-uri-path (hunchentoot:script-name request))
           (vom:debug "back from auth server with params: ~A~%"
                      (hunchentoot:get-parameters request))
           (lambda ()
             ;; back from the authorization server... exchange code for token
             (assert hunchentoot:*session*)
             (assert (hunchentoot:session-value original-url-session-key))
             (let* ((original-url
                     (or (hunchentoot:session-value original-url-session-key) "/"))
                    (code (-> (hunchentoot:get-parameters request)
                              (assoq "code")))
                    (resp-token (exchange-code-for-token code oauth-client)))
               (if (resp-token-access-token resp-token)
                   (progn
                     (setf (hunchentoot:session-value login-session-key)
                           (make-api-login
                            :key nil
                            :token resp-token))
                     (when on-authenticated-fn
                       (funcall on-authenticated-fn
                                (hunchentoot:session-value login-session-key)))
                     (hunchentoot:redirect original-url))
                   (progn (setf (hunchentoot:return-code*)
                                hunchentoot:+http-authorization-required+)
                          ;; TODO return json
                          (vom:info "token request rejected: ~A~%" resp-token)
                          (erjoalgo-webutil:json-resp
                           `((:error . "token request rejected")
                             ;; cl-json complains:
                             ;; (my-clos-object) is not of a type which can be encoded by ENCODE-JSON...
                             ;; use another json library
                             (:details .  ,(prin1-to-string resp-token)))))))))
          (t
           (assert (not (authenticated?)))
           (lambda ()
             ;; missing login, redirect to remote auth server
             (unless hunchentoot:*session*
               (hunchentoot:start-session))
             (setf (hunchentoot:session-value original-url-session-key)
                   (hunchentoot:request-uri request))
             (let* ((local-auth-url
                     (format nil "~A://~A~A"
                             ;; (hunchentoot:server-protocol request)
                             scheme
                             (hunchentoot:host)
                             oauth-authorize-uri-path))
                    (remote-auth-url (auth-server-redirect-url
                                      oauth-client
                                      local-auth-url
                                      scopes-to-request)))
               (vom:debug "redirecting to remote oauth: ~A~%"
                          remote-auth-url)
               (vom:debug "local redirect url: ~A~%"
                          local-auth-url)
               (hunchentoot:redirect remote-auth-url)))))))))
