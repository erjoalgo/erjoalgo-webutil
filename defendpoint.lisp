(in-package #:erjoalgo-webutil)
;; the keywords below whose default forms are undefined symbols
;; should be either provided explicitly
;; or their defaults let-bound at compile time via COMPILER-LET
;; TODO muffle warnings
;; there should be 7 warnings
(defmacro defendpoint (name
                       &key (base-url base-url)
                         (req-update req-update)
                         (method method)
                         (resource resource)
                         make-http-request-extra-args
                         (depaginator depaginator)
                         (authenticator authenticator)
                         (api-req-extra-args-compile-time
                          api-req-extra-args-compile-time))
  ;; "Defines a function FUN-SYM that calls an api-endpoint.
  ;;  DEFAULT-PARAMS specifies defaults parameters used in the request
  ;;  unless overridden later by the caller."
  (assert resource)
  (let ((api-req-extra-args-runtime-sym (gensym "api-req-extra-args-runtime-")))
    (destructuring-bind (lambda-list http-request-sym update-http-request-form)
        (or req-update `(nil ,(gensym "http-request-") nil))
      (vom:debug "lambda-list: ~A~%" lambda-list)
      `(defun ,name ,(append lambda-list `(&rest ,api-req-extra-args-runtime-sym))
         ;; ,(format nil "~A ~A/~A ~A" method api-base-url resource (or default-params ""))
         (let ((,http-request-sym
                (make-http-request :method ,method :resource ,resource
                                   ,@make-http-request-extra-args)))
           ,update-http-request-form
           ,(let ((args `(,http-request-sym
                                  (append
                                   ,api-req-extra-args-runtime-sym
                                   ',api-req-extra-args-compile-time
                                   (list :api-base-url ,base-url
                                     :depaginator ',depaginator
                                     :authenticator ',authenticator)
                                  ))))
              (vom:debug "args ~A~%" args)
              `(progn
                 (vom:debug "runtime args ~A~%" ',args)
                 (apply 'api-req ,@args))))))))
