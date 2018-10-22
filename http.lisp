(in-package #:erjoalgo-webutil)

(defmacro retry-times (n timeout-secs &body body)
  "Retry form N times, with each retry timed out at TIMEOUT-SECS.
   An error is raised only after all retries have been exhausted."
  (let ((i-sym (gensym "i"))
        (ex-sym (gensym "ex"))
        (loop-ex-sym (gensym "loop-ex"))
        (loop-tag-sym (gensym "loop-tag"))
        (timeout-secs (or timeout-secs 1)))
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

(defmacro define-regexp-route (name (uri-regexp &rest capture-names)
                                                  docstring
                                                  ;; condition
                               &body body)
  ;; taken from IRC
  "Define a hunchentoot handler `name' for paths matching `uri-regexp'.
   An optional list `capture-names' may be provided to capture path variables.
   The capturing behavior is based on wrapping `ppcre:register-groups-bind'"

  (let ((dispatcher-name (intern
                          (format nil "HUNCHENTOOT-DISPATCHER-~A" (symbol-name name))))
        (scanner-sym (gensym "SCANNER-"))
        (request-sym (gensym "REQUEST-")))
    ;; (declare (ignore scanner))
    `(progn
       (defvar ,scanner-sym
         (ppcre:create-scanner ,uri-regexp))
       (defun ,name ()
         ,docstring
         (log-request ,(format nil "matching ~A"
                               dispatcher-name))
         (ppcre:register-groups-bind ,capture-names
             (,uri-regexp (hunchentoot:script-name*))
           (log-request ,(format nil "matched ~A"
                                 dispatcher-name))
           ,@body))
       (defun ,dispatcher-name (,request-sym)
         (log-request ,(format nil "on dispatcher ~A" dispatcher-name))
         (when (and ;;,condition
                (ppcre:scan ,scanner-sym (hunchentoot:script-name ,request-sym)))
           ;; (ppcre:scan (ppcre:create-scanner ,uri-regexp) (hunchentoot:script-name ,request-sym)))
           (log-request ,(format nil "on handler ~A" name))
           ',name))
       (push ',dispatcher-name
             hunchentoot:*dispatch-table*))))

(defun json-resp (body &key (return-code 200))
  "Convert a lisp object into a json response with the appropriate content type
to be called within a hunchentoot handler. "

  (setf (hunchentoot:return-code*) return-code)
  (setf (hunchentoot:content-type*) "application/json")

  ;; (cl-json:encode-json body)
  ;; https://tbnl-devel.common-lisp.narkive.com/CO37ACWN/
  ;; hunchentoot-devel-how-to-properly-write-directly-to-output-stream
  (let ((out (flex:make-flexi-stream (hunchentoot:send-headers)
                                     :external-format
                                     hunchentoot:*hunchentoot-default-external-format*
                                     :element-type 'character)))
    (cl-json:encode-json body out)))

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

(defun first-file-with-extension (directory ext)
  "Locate the first file in DIRECTORY matching the extension EXT."
  (loop for path in (uiop:directory-files directory)
     thereis (and (equal ext (pathname-type path)) path)))

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
