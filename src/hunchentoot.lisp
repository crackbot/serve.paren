
(defpackage #:serve.paren-hunchentoot
  (:use :cl :alexandria :serve.paren :hunchentoot :split-sequence))

(in-package #:serve.paren-hunchentoot)

;; [TODO] Add documentation

(defparameter *bundle-name* "bundle.paren"
  "[TODO] Documentation")

(defun uri-to-lib (uri)
  (let* ((parts (split-sequence:split-sequence #\: uri))
         (host-parts (split-sequence:split-sequence #\/ (car parts))))
    (when (> (length parts) 1)
      (values (car (last host-parts))
              (car (last parts))))))

(defmethod hunchentoot:handle-request :around ((acceptor hunchentoot:acceptor)
                                               (request hunchentoot:request))
  (flet ((return-js (js-content)
           (setf (hunchentoot:content-type*) "text/javascript"
                 (hunchentoot:return-code*) 200)
           js-content))
    (multiple-value-bind (libname filename)
        (uri-to-lib (hunchentoot:request-uri request))
      (if (and libname filename)
        (let ((lib (find-pslib libname)))
          (if (string= filename *bundle-name*)
              (return-js (serve.paren:compile-pslib lib))
              (when-let ((compiled-js (serve.paren:compile-psfile lib filename)))
                (return-js compiled-js))))
        (call-next-method)))))
      
