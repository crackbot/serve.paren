
(in-package :serve.paren)

(defsection recepies (:title "Some most common use cases.")
  (recepie1 section)
  (recepie2 section)
  (recepie3 section)
  (recepie4 section)
  (recepie5 section)
  (recepie6 section)
  (recepie7 section)
  (recepie8 section))

(defsection recepie1 (:title "I want to quickly use some parenscript forms")
  "Use :runtime option"
  
  "```cl-transcript
  (defparameter *ps-forms*
    '(defun double (x)
      (+ x x)))
  
  (defpslib \"mylib\"
      :runtime *ps-forms*)
  ```"
  
  "Then when you want to include above into your html page call
  COMPILE-PSRUNTIME")

(defsection recepie2 (:title "I have one or couple of paren files")
  "Pass :files directly"

  "```cl-transcript
  (defpslib \"mylib2\"
      :files (list #p\"path/to/file.paren\" #p\"another/file.paren\")
      :system-source-dir #p\"/path/to/your/project\")
  ```"

  "To compile above call COMPILE-PSLIB or for individual files use
COMPILE-PSFILE. Note that files specificed in :files should reside
in :system-source-dir.")

(defsection recepie3 (:title "I have project with structure and lots of paren files")
  "Use :paren-source-dir"

  "```cl-transcript
  (defpslib \"mylib3\"
      :system-source-dir #p\"/path/to/project\"
      :paren-source-dir #p\"public/paren\")
  ```")

(defsection recepie4 (:title "I have lots of paren files and js files
and those are located in different places")
  "Specify multiple source dirs in :paren-source-dir and use :include-js option"
  
  "```cl-transcript
  (defpslib \"mylib4\"
      :system-source-dir #p\"/path/to/project\"
      :paren-source-dir (list #p\"public/paren\" #p\"public/js\")
      :inlcude-js)
  ```"
  
  "Also take a look at PSLIB-FILES it will let you configure the order
of files.")

(defsection recepie5 (:title "I want to do something after or before compiling the lib")
  "Create your own pslib class and modify PSLIB-COMPILE method")

(defsection recepie6 (:title "I want to so something special to just one file durtion compilation")
  "Add a PSLIB-COMPILE-PSFILE method with an eql specializer")
  
(defsection recepie7 (:title "I want my system to be easily distributable")
  "Define it as ASDF system with :parenscript-file compontents")

(defsection recepie8 (:title "I want to easily setup paren->js and access it via web")
  "Use hunchentoot adapter")

(defsection recepie9 (:title "I need to tune the order of files")
  )

;; (defclass my-pslib (pslib)
;;   ((additional-option :initarg :my-option)))

;; (defmethod initialize-instance :after ((lib pslib))
;;   )

;; (defpslib "test"
;;     :cls 'my-pslib
;;     :system-source-dir #p"")
