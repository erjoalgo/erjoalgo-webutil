(in-package #:erjoalgo-webutil)

(export
 '(make-from-json-alist
   with-json-paths
   json-get-nested
   -json-get-nested
   json-key-to-lisp
   lisp-to-json-key
   json-path-split))

;; TODO
(setf cl-json:*json-identifier-name-to-lisp* 'json-key-to-lisp)

(defmacro make-from-json-alist (json-alist type)
  "Make an instance of type TYPE from a cl-json-decoded JSON-ALIST."
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

(defmacro with-json-paths (obj var-paths &body body)
  "Let-bind certain json paths within json OBJ.
   VAR-PATHS has the form (VAR JSON-PATH). See test for examples."
  `(let ,(loop for (var path) in var-paths collect
              `(,var (json-get-nested ,obj ,path)))
     ,@body))

(defun json-get-nested (alist path)
  "Get the value in ALIST at the given PATH."
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

(defmacro -json-get-nested (alist path)
  "Macro-version of JSON-GET-NESTED. Only macroexpands if
   PATH is a string known at compile time."
  `(json-get-nested ,alist
                    ,(if (stringp path)
                         `',(json-path-split path)
                         `(json-path-split ,path))))

(defun json-key-to-lisp (key)
  "Convert a json object KEY string into a lisp identifier name."
  ;; TODO first apply CAMEL-CASE-TO-LISP in case keys are camel case
  (string-upcase (cl-ppcre:regex-replace-all
                  "--"
                  (cl-json:camel-case-to-lisp key)
                  "-")))

(defun lisp-to-json-key (lisp-identifier)
  "Convert a LISP-IDENTIFIER string name into a json object key string."
  (cl-json:lisp-to-camel-case (symbol-name lisp-identifier)))

(defun json-path-split (path)
  "Split a json PATH string into components."
  (mapcar (lambda (attr)
            (if (DIGIT-CHAR-P (aref attr 0))
                (parse-integer attr)
                (intern (json-key-to-lisp attr) :keyword)))
          (cl-ppcre:split "][.]|[][.]" path)))
