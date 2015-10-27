
(defpackage :serve.paren
  (:use :cl :split-sequence :alexandria :mgl-pax)
  (:nicknames "serve")
  (:export :defpslib
           
           :compile-pslib
           :compile-psfile
           :compile-psruntime
           
           :all-files-urls
           :concrete-files-urls

           :find-pslib)
  (:import-from :alexandria :if-let :when-let :hash-table-keys))

(defpackage #:serve.paren.asdf
  (:use :cl :parenscript :asdf :mgl-pax)
  (:documentation "ASDF extensions that help compile and use Parenscript systems.")
  (:export #:compile-script-system))

;; (defpackage :serve.paren-tests
;;   (:use :cl :serve.paren :parenscript :lisp-unit))
