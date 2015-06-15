
(defpackage #:serve.paren-hunchentoot
  (:use :cl :alexandria :serve.paren :hunchentoot))

(in-package #:serve.paren-hunchentoot)

(defmethod hunchentoot:handle-request :before ((acceptor hunchentoot:acceptor)
                                               (request hunchentoot:request))
  )
