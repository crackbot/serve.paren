
(defsystem #:serve.paren
  :description "Simple way to define and serve parenscript libs in webapps."
  :name "serve.paren"
  :version "0.0.3"
  :author "Crackbot <thecrackbot@gmail.com>"
  :maintainer "Crackbot <thecrackbot@gmail.com>"
  :license "The MIT License (MIT)"
  :components ((:static-file "serve.paren.asd")
               (:module "src"
                        :components ((:file "package")
                                     (:file "serve")
                                     (:file "recepies")
                                     (:file "api")
                                     (:file "hunchentoot")
                                     ;(:file "restas")
                                     (:file "asdf"))))
  :depends-on (:cl-fad :hunchentoot :split-sequence :paren-files :alexandria :mgl-pax))
