(in-package #:erjoalgo-webutil)

(defun read-file (filename)
  "Read file contents into a string."
  (with-output-to-string (out)
    (with-open-file (in filename)
      (format out "~{~A~^~%~}"
              (loop as line = (read-line in nil)
                 while line
                 collect line)))))

(defmacro alist-get (alist item)
  "get ITEM in ALIST"
  `(cdr (assoc ,item ,alist :test 'equal)))

(defmacro -> (&rest forms)
  "Threading macro."
  (if (cadr forms)
      (destructuring-bind (first second . rest) forms
	(destructuring-bind (a . a-rest) (if (atom second)
					     (cons second nil)
                                             second)
	  `(-> ,(apply 'list a first a-rest) ,@rest)))
      (car forms)))

(defmacro ->> (&rest forms)
  (if (cadr forms)
      (destructuring-bind (a b . rest) forms
        `(->> ,(append
                (if (atom b) (cons b nil) b)
                (list a))
              ,@rest))
      (car forms)))

(defun make-directories-recursively (dir &key (mode #o755))
  (unless (probe-file dir)
    (make-directories-recursively
     (uiop:pathname-parent-directory-pathname
      (uiop:ensure-directory-pathname dir))
     :mode mode)
    (sb-posix:mkdir dir mode)))

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for sym in syms
            collect (list sym `(gensym ,(format nil "~A-" (symbol-name sym)))))
     ,@body))

(defmacro slot-value-> (obj slots)
  (if slots
      `(slot-value-> (slot-value ,obj ',(car slots)) ,(cdr slots))
      obj))
