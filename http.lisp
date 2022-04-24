(in-package #:erjoalgo-webutil)

(defmacro retry-times (n timeout-secs &body body)
  "Retry form N times, with each retry timed out at TIMEOUT-SECS.
   An error is raised only after all retries have been exhausted."
  (with-gensyms (i-sym ex-sym loop-ex-sym loop-tag-sym)
    `(loop
        named ,loop-tag-sym
        with ,loop-ex-sym = nil
        for ,i-sym below ,n
        do (vom:debug "~A ~A~%" ,i-sym ,loop-ex-sym)
        do
          (handler-case
              (progn
                (setf ,loop-ex-sym nil)
                (return-from ,loop-tag-sym
                  (progn ,@body)))
            (error (,ex-sym)
              (setf ,loop-ex-sym ,ex-sym)
              (vom:debug "~A failed with ~A retrying ~D / ~D... ~%"
                         ',body ,ex-sym (1+ ,i-sym) ,n)
              (sleep ,timeout-secs)))
        while ,loop-ex-sym
        finally (error ,loop-ex-sym))))

(defun drakma-json-content-type-hack (&optional remove)
  "Remove application/json from drakma:*text-content-types*."
  (let ((json (cons "application" "json")))
    (setf drakma:*text-content-types*
          (delete json drakma:*text-content-types* :test 'equal))
    (unless remove (push json drakma:*text-content-types*))
    drakma:*text-content-types*))

(defun params (&rest flat)
  "Convert a flat list of key-value pairs into an alist."
  (loop for (k v) on flat by #'cddr collect (cons k v)))

(defmacro -params (&rest flat)
  "Convert a flat list of key-value pairs into an alist."
  (params flat))

(defmacro defroutes (var &rest routes)
  "Define a hunchentoot dispatch table VAR of http routes.
   Each route has the form (ALLOWED-METHODS URI-REGEXP . CAPTURE-NAMES):

   ALLOWED-METHODS may be a list of http verbs as in (:GET :POST), or the
   special value t which matches any verb.

   URI-REGEXP and CAPTURE-NAMES are used to match against the request path
   and capture path variables. They correspond to the REGEX and VAR-LIST
   parameters in PPCRE:REGISTER-GROUPS-BIND.

   For example,
    - URI-REGEXP \"/user/([0-9]+)/product/([0-9]+)\"
    - CAPTURE-NAMES (user-id product-id)
    would capture the user-id and product-id path variables of
    requests matching the given path regexp.

   - CAPTURE-NAMES could also have the form
     ((#'PARSE-INTEGER UNIQUE-ID) (#'PARSE-INTEGER PRODUCT-ID))
    as supported by PPCRE:REGISTER-GROUPS-BIND."

  `(progn
     (defparameter ,var nil)
     ,@(loop for ((allowed-methods uri-regexp . capture-names) . body) in (reverse routes)
          as sanitized =
            (string-upcase (cl-ppcre:regex-replace-all "[^a-zA-Z0-9]" uri-regexp "-"))
          as scanner-sym = (gensym (format nil "SCANNER-~A-" sanitized))
          as dispatcher-sym = (gensym (format nil "DISPATCHER-~A-" sanitized))
          as handler-sym = (gensym (format nil "HANDLER-~A-" sanitized))
          as request-sym = (gensym "REQUEST-")
          as lambda-list = (loop for arg in capture-names
                              collect (if (consp arg)
                                          ;; ppcre:register-groups-bind allows a
                                          ;; (fn sym) form here
                                          (cadr arg)
                                          arg))

          append
            `((defvar ,scanner-sym
                (ppcre:create-scanner ,uri-regexp))
              (defun ,handler-sym ()
                (ppcre:register-groups-bind ,capture-names
                    (,scanner-sym (hunchentoot:script-name*))
                  (log-request ,(format nil "matched ~A" dispatcher-sym))
                  ,@body))
              (defun ,dispatcher-sym (,request-sym)
                (log-request ,(format nil "matching ~A" dispatcher-sym))
                (when (and ,@(unless (eq t allowed-methods)
                               `((member (hunchentoot:request-method ,request-sym)
                                         ',allowed-methods)))
                           (ppcre:scan ,scanner-sym
                                        (hunchentoot:script-name ,request-sym)))
                  ',handler-sym))
              (push ',dispatcher-sym ,var)))))

(defroutes test-routes
    (((:get) "/health") "OK")

  (((:get) "/privacy") "privacy policy")

  ((t "/echo") (format nil "the method used was ~A"
                       (hunchentoot:request-method
                        hunchentoot:*request*)))

  (((:get) "/user/([0-9]+)" (#'parse-integer user-id))
   (format nil "one plus this user id is ~D" (1+ user-id))))


(defun json-resp (body &key (return-code 200)
                         (encoding-fn 'cl-json:encode-json-to-string))
  "Convert a lisp object into a json response with the appropriate content type
to be called within a hunchentoot handler. "

  (setf (hunchentoot:return-code*) return-code)
  (setf (hunchentoot:content-type*) "application/json")

  ;; TODO consider writing json directly to stream
  ;; (encoding-fn 'cl-json:encode-json-to)
  ;; https://tbnl-devel.common-lisp.narkive.com/CO37ACWN/
  ;; hunchentoot-devel-how-to-properly-write-directly-to-output-stream
  '(let ((out (flex:make-flexi-stream (hunchentoot:send-headers)
                                     :external-format
                                     hunchentoot:*hunchentoot-default-external-format*
                                     :element-type 'character)))
    (funcall encoding-fn body out))
  (funcall encoding-fn body))

(defun json-req ()
  "Obtain the current request payload as a decoded json object."
  (let* ((json-string (hunchentoot:raw-post-data :force-text t))
         (json (cl-json:decode-json-from-string json-string)))
    json))

(defmacro check-nonnil (form)
  ;; TODO multiple values
  "Asserts a form is non-nil."
  (let ((val-sym (gensym "val")))
    `(let ((,val-sym ,form))
       (assert ,val-sym nil "assertion ~A failed" ',form)
       ,val-sym)))

(defun first-file-matching (pathname-maybe-wild)
  "Locate the first file in DIRECTORY matching the extension EXT."
  (if (wild-pathname-p pathname-maybe-wild)
      (check-nonnil
       (car (directory pathname-maybe-wild)))
      (truename pathname-maybe-wild)))

(defun hunchentoot-make-add-fake-session (data user-agent
                                          &key
                                            (real-remote-addr "127.0.0.1"))
  "Create a 'fake' hunchentoot session with mock data DATA as HUNCHENTOOT::SESSION-DATA.
   Returns (VALUES SESSION COOKIE).

   The resulting SESSION will be actived for a given request if:
     - request user-agent is USER-AGENT
     - request real-remote-addr (IP address?) is REAL-REMOTE-ADDR
     - the request contains a header with the name 'cookie' and value COOKIE"

  (let* ((hunchentoot:*acceptor* nil)
         (hunchentoot:*request*
          (make-instance 'hunchentoot:request
                         :uri ""
                         :headers-in `((:user-agent ,user-agent))
                         :acceptor nil
                         :remote-addr real-remote-addr))
         (sess (make-instance 'HUNCHENTOOT:SESSION))
         (id (slot-value sess 'HUNCHENTOOT::SESSION-ID)))

    (setf (slot-value sess 'HUNCHENTOOT::SESSION-DATA) data)
    (push (cons id sess) hunchentoot::*session-db*)
    (let* ((string
            (HUNCHENTOOT::encode-session-string id
                                                user-agent
                                                real-remote-addr
                                                (HUNCHENTOOT::session-start sess))))
      (setf (slot-value sess 'HUNCHENTOOT::session-string) string)
      (values sess (HUNCHENTOOT:session-cookie-value sess)))))

(defmacro with-mock ((fname fun) &body body)
  ;; from https://stackoverflow.com/questions/3074812/
  "Shadow the function named fname with fun
   Any call to fname within body will use fun, instead of the default function for fname.
   This macro is intentionally unhygienic:
   fun-orig is the anaphor, and can be used in body to access the shadowed function"
  `(let ((fun-orig))
     (cond ((fboundp ',fname)
            (setf fun-orig (symbol-function ',fname))
            (setf (symbol-function ',fname) ,fun)
            (unwind-protect (progn ,@body)
              (setf (symbol-function ',fname) fun-orig)))
           (t
            (setf (symbol-function ',fname) ,fun)
            (unwind-protect (progn ,@body)
              (fmakunbound ',fname))))))

(defun log-request (context-string
                    &key (request hunchentoot:*request*))
  (vom:debug "~A: ~A ~A~A ~A (~A)"
             context-string

             (hunchentoot:request-method request)
             (hunchentoot:host request)
             (hunchentoot:script-name request)
             (hunchentoot:get-parameters request)

             hunchentoot:*session*))
