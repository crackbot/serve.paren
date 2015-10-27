
(in-package :serve.paren)

(defsection @main-manual (:title "Manual for serving Parenscript")
  (serve.paren asdf:system)
  
  "serve.paren package lets you define parenscript libraries, and
either serve them using one of web server hookups or produce an
optimized and compressed version of resulting javascript code"

  (@deflib-manual section)
  (@compile-manual section)
  (@serve-manual section)
  ;(@recepies section)
  )

(defsection @deflib-manual (:title "How to define a library")
  "There are two general ways of how to define a parenscript library,
which one to use mostly depends on your taste and usecase."
  
  ;(serve.paren.asdf::@asdf-manual section)
  (defpslib function)

  "NOTE: Personally i usually start with defpslib form, because it
  allows faster development cycle, adding/removing files from library
  definion is easy. But when project gets mature enough i move
  definition to asdf system, which makes deploying and sharing easier.")

;; Define

(defun defpssyslib (libname system-name &key
                                 package
                                 system-source-dir
                                 (file-match-fn #'match-ps-file)
                                 (cls 'pslib))
  "Define new parenscript library from asdf system."
  (let* ((system (asdf:find-system system-name))
         (source-dir (or system-source-dir
                         (asdf:system-source-directory system)))
         (lib (make-instance cls
                             :name libname
                             :paren-source-dir source-dir
                             :runtime nil
                             :package package
                             :match-fn file-match-fn)))
    (when (pathnamep source-dir)
      (setf (slot-value lib 'system-source-dir)
            (truename source-dir)))
      (setf (gethash libname *libs*) lib)
      (setf (pslib-files lib)
            (serve.paren.asdf::system-ps-files system))))

(defun defpslib (libname &key system-source-dir
                          paren-source-dir
                          runtime
                          files
                          package
                          (custom-files-order #'identity)
                          (file-match-fn #'match-ps-file)
                          (include-js nil)
                          (watch-changes nil)
                          (cls 'pslib))
  "Define new parenscript library. The ps source files may come from few
different places described below.

:system-source-dir should always be set unless you use only :runtime key
\(and no files at all\), it should be equal to project source directory, and
is used to ...

:files is used to directly specify files that you want to be included
into library, all of the included files should be located inside
of :system-source-dir

If :paren-source-dir key is provided it walks recursively through
directory looking for any files with *PS-EXT* extension.
If :inlcude-js option is set then it will also grab *JS-EXT* files.
If :watch-changes options is set then it will look for changes
inside :paren-source-dir

Use :package to set package when compiling parenscript library.

With :runtime key you can specify parenscript forms that can be
included in the lib directly \(without use of files\).
:file-match-fn lets you control how files are matched when compiling
the library, look at match-file."
  ;; [TODO] check that paren-source-dir & files are children of
  ;; system-source-dir
  ;; [TODO] move some of it into initialize-instance :after method
  (flet ((scan-for-files (ext)
           (cond ((listp paren-source-dir)
                  (mappend #'(lambda (dir)
                               (files-with-ext dir ext :recursive t))
                           paren-source-dir))
                 (t (files-with-ext paren-source-dir ext :recursive t)))))
    (let* ((lib (make-instance cls
                               :name libname
                               :paren-source-dir paren-source-dir
                               :runtime runtime
                               :package package
                               :match-fn file-match-fn))
           (all-files (append files
                              (when paren-source-dir
                                (scan-for-files *ps-ext*))
                              (when (and include-js paren-source-dir)
                                (scan-for-files *js-ext*)))))
      (when (pathnamep system-source-dir)
        (setf (slot-value lib 'system-source-dir)
              (truename system-source-dir)))
      (setf (gethash libname *libs*) lib)
      (setf (pslib-files lib)
            (funcall custom-files-order
                     (reverse (mapcar (curry #'relative-filename lib) all-files)))))))

;; Compile

(defsection @compile-manual (:title "How to compile a library")
  (compile-pslib function)
  (compile-psfile function)
  (compile-psruntime (method () (pslib))))

(defmethod compile-psruntime ((lib pslib))
  "Compile lib runtime forms."
  (let ((*package* (or (find-package (pslib-package lib)) *package*))
        (runtime (pslib-runtime lib)))
    (ps:ps* runtime)))

(defun compile-pslib (lib &key output minify)
  "Compile the whole lib returning javascript as a simple string."
  (let ((res (pslib-compile lib)))
    (when minify
      (setq res (js-minify res)))
    (if (pathnamep output)
        (write-file output res)
        res)))

(defun compile-psfile (lib filename &key package output minify)
  (let ((*package* (or package
                       (find-package (pslib-package lib))
                       *package*)))
    (let ((res (pslib-compile-psfile lib filename)))
      (when minify
        (setq res (js-minify res)))
      (if (pathnamep output)
          (write-file output res)
          res))))

;; URLs

(defsection @serve-manual (:title "How to serve it via web")
  (all-files-urls function)
  (concrete-files-urls function))

(defun concrete-files-urls (lib files &key host)
  "Build urls for specific files, also look at ALL-FILES-URLS"
  (let ((urls (files-urls lib files)))
    (if host
        (mapcar (curry #'concatenate-url host) urls)
        urls)))

(defun all-files-urls (lib &key host)
  "Build urls for all paren lib files, if host is provided it will
output absolute urls if not then relative. Those relative urls should
work with provided HUNCHENTOOT and RESTAS modules."
  (let ((urls (pslib-urls lib)))
    (if host
        (mapcar (curry #'concatenate-url host) urls)
        urls)))

;; Helper

(defun find-pslib (libname)
  "Get lib by name"
  (or (gethash libname *libs*)
      (error "No pslib ~A found, available libs: ~{\"~A\", ~}" libname (defined-lib-names))))

(defun js-first-order (files)
  "Custom files order, js files are coming first"
  (sort files
        (lambda (a b)
          (declare (ignore b))
          (when (string-ends-with *js-ext* (namestring a))
            t))))
