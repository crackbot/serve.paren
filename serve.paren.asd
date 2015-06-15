
(defsystem #:serve.paren
  :description "Simple way to define and serve parenscript libs in webapps."
  :name "serve.paren"
  :version "0.0.1"
  :author "Crackbot <thecrackbot@gmail.com>"
  :maintainer "Crackbot <thecrackbot@gmail.com>"
  :license "The MIT License (MIT)"
  :components ((:static-file "serve.paren.asd")
               (:module "src"
                        (:components ((:file "package")
                                      (:file "serve")
                                      (:file "hunchentoot")
                                      (:file "restas")))))
  :depends-on (:iterate :restas :split-sequence :paren-files :alexandria :mgl-pax))
