(in-package #:yt-comments/util)

(defun json-key-to-lisp (key)
  ;; TODO first apply CAMEL-CASE-TO-LISP in case keys are camel case
  (string-upcase (cl-ppcre:regex-replace-all
                  "--"
                  (cl-json:camel-case-to-lisp key)
                  "-")))

(defun lisp-to-json-key (lisp-identifier)
  (cl-json:lisp-to-camel-case (symbol-name lisp-identifier)))

(setf cl-json:*json-identifier-name-to-lisp*
      'json-key-to-lisp)

(defmacro -> (&rest forms)
  (if (cadr forms)
      (destructuring-bind (first second . rest) forms
	(destructuring-bind (a . a-rest) (if (atom second)
					     (cons second nil)
                                             second)
	  `(-> ,(apply 'list a first a-rest) ,@rest)))
      (car forms)))

(defmacro make-from-json-alist (json-alist type)
  (let ((slots (loop for slot in (sb-mop:class-direct-slots (find-class type))
                  collect (slot-value slot 'SB-PCL::NAME)))
        (instance (gensym "instance"))
        (k (gensym "k"))
        (v (gensym "v"))
        (slot-sym (gensym "slot-sym"))
        (class-package (symbol-package type)))
    `(progn
       (loop
          with ,instance = (make-instance ',type)
          for (,k . ,v) in ,json-alist
          as ,slot-sym = (intern (symbol-name ,k) ,class-package)
          do (if (member ,slot-sym ',slots)
                 (setf (slot-value ,instance ,slot-sym) ,v)
                 (warn "missing slot ~A in type ~A" ,slot-sym ',type))
          finally (return ,instance)))))

(defun json-path-split (path)
  (mapcar (lambda (attr)
            (if (DIGIT-CHAR-P (aref attr 0))
                (parse-integer attr)
                (intern (json-key-to-lisp attr) :keyword)))
          (cl-ppcre:split "][.]|[][.]" path)))

(defun json-get-nested (alist path)
  (when (stringp path)
    (setf path (json-path-split path)))
  (when (atom path)
    (setf path (list path)))
  (reduce (lambda (alist attr)
            (typecase attr
              (number (nth attr alist))
              (keyword (cdr (assoc attr alist)))
              (t (error "invalid type for path component: ~A" attr))))
          path :initial-value alist))

(defmacro json-get-nested-macro (alist path)
  `(json-get-nested ,alist
                    ,(if (stringp path)
                         `',(json-path-split path)
                         `(json-path-split ,path))))

(defmacro with-json-paths (obj var-paths &body body)
  `(let ,(loop for (var path) in var-paths collect
              `(,var (json-get-nested ,obj ,path)))
     ,@body))

(defun drakma-json-content-type-hack (&optional remove)
  (let ((json (cons "application" "json")))
    (setf drakma:*text-content-types*
          (delete json drakma:*text-content-types* :test 'equal))
    (unless remove (push json drakma:*text-content-types*))
    drakma:*text-content-types*))

(defun read-file (filename)
  (with-output-to-string (out)
    (with-open-file (in filename)
      (format out "~{~A~^~%~}"
              (loop as line = (read-line in nil)
                 while line
                 collect line)))))

(defmacro retry-times (n timeout-secs &body body)
  (let ((i-sym (gensym "i"))
        (ex-sym (gensym "ex"))
        (loop-ex-sym (gensym "loop-ex"))
        (loop-tag-sym (gensym "loop-tag"))
        (timeout-secs (or timeout-secs 1)))
    `(loop
        named ,loop-tag-sym
        with ,loop-ex-sym = nil
        for ,i-sym below ,n
        do (format t "~A ~A~%" ,i-sym ,loop-ex-sym)
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

(defmacro assoq (alist item)
  `(cdr (assoc ,item ,alist :test 'equal)))

(defun params (&rest flat)
  "Convert a flat list of key-value pairs into an alist."
  (loop for (k v) on flat by #'cddr collect (cons k v)))

(defmacro -params (&rest flat)
  (params flat))
