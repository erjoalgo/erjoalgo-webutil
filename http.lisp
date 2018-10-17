(in-package #:erjoalgo-webutil)

(export
 '(retry-times
   drakma-json-content-type-hack
   params
   -params))

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
              (format nil "failed with ~A retrying ~D/~D... ~%"
                      ,ex-sym (1+ ,i-sym) ,n)
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
