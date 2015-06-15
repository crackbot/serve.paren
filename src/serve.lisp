
(in-package :serve.paren)

(defsection @main-manual (:title "Manual for serving PS")
  (serve.paren asdf:system)
  
  "serve.paren package lets you define parenscript libraries, and
either serve them using one of web server hookups or produce an
optimized and compressed version of resulting javascript code"

  (@deflib-manual section)
  (@compile-manual section)
  (@serve-manual section))

(defsection @deflib-manual (:title "How to define a library")
  (defpslib function))

(defsection @compile-manual (:title "How to compile a library")
  (compile-pslib function)
  (compile-psfile function)
  (compile-psruntime function))

(defsection @serve-manual (:title "How to serve it via web")
  (all-files-urls function)
  (concrete-files-urls function))

(defparameter *libs* (make-hash-table :test #'equal))
(defparameter *ps-ext* ".paren")

(defstruct pslib system
                 package
                 source-directory
                 name
                 runtime
                 (files nil :type list)
                 (path nil :type (or null pathname))
                 (match-fn nil :type function))

(defun defpslib (libname &key system source-directory package runtime pathname
                           (files ())
                           (file-match-fn #'match-ps-file))
"Define new parenscript library. The ps source files may come from few
different places described below.

If :system key is provided it will lookup asdf system and find grab
files which were included via :parenscript-file (that extension is
provided by paren-files library).

If :pathname key is provided it walks recursively through directory
looking for any files with *PS-EXT* extension.

You can also specify files directly via :files key.

:package key is used to set package when compiling parenscript library
by using :runtime key you can specify parenscript forms that can be
included in the lib directly, without using any files

:file-match-fn let you controll how files are matched when compiling
the library"
  (let* ((lib (make-pslib :system (and system (asdf:find-system system))
                          :source-directory source-directory
                          :name libname
                          :path pathname
                          :package package
                          :runtime runtime
                          :match-fn file-match-fn))
         (all-files (append files
                            (when system (system-ps-files (pslib-system lib)))
                            (when pathname
                              (files-with-ext pathname *ps-ext* :recursive t)))))
    
    (setf (gethash libname *libs*) lib)
    (setf (pslib-files lib)
          (reverse (mapcar (curry #'relative-filename libname) all-files)))))
    
    ;; (when system
    ;;   (setf all-files (append all-files (system-ps-files (pslib-system lib)))))
    
    ;; (when source-directory
    ;;   (setf all-files (append all-files (path-files-filtered source-directory *ps-ext*))))

    ;; (setf (pslib-files lib)
    ;;       (reverse (mapcar (curry #'relative-filename name) all-files)))))

;; urls

(defun file-url (libname psfile)
  (concatenate 'string libname ":" (namestring (relative-filename libname psfile))))

(defun concrete-files-urls (libname files)
  "Build urls for specific files, also look at ALL-FILES-URLS"
  (mapcar (curry #'file-url libname) files))

(defun all-files-urls (libname &key host)
  "Build urls for all paren lib files, if host is provided it will output absolute urls if not then relative. Those relative urls should work when calling COMPILE-PSFILE"
  (mapcar (curry #'file-url libname) (pslib-files (lookup-lib libname))))

;; lookup lib data

(defun defined-lib-names ()
  (hash-table-keys *libs*))

(defun lookup-lib (libname)
  "Get parenlib by name"
  (or (gethash libname *libs*)
      (error "No pslib ~A found, available libs: ~{\"~A\", ~}" libname (defined-lib-names))))

(defun lookup-pkg (libname)
  "Get package in parenlib"
  (pslib-package (lookup-lib libname)))

(defun pslib-source-dir (pslib)
  ""
  (or (pslib-source-directory pslib)
      (asdf:system-source-directory (pslib-system pslib))))

;; 

(defun extract-subpath (path1 path2)
  "Extract path1 from path2"
  (pathname (subseq (namestring path2) (length (namestring path1)))))

(defun relative-filename (libname filename)
  "Given filename with full pathname we try to build relative pathname
   accodring to system definition registry"
  (let* ((lib (lookup-lib libname))
         (source-directory (pslib-source-dir lib)))
    (if (cl-fad:pathname-relative-p filename)
        filename
        (extract-subpath source-directory filename))))

(defun match-file (libname filename)
  "Try to match files in lib against name"
  (let ((lib (lookup-lib libname)))
    (funcall (pslib-match-fn lib) filename (pslib-files lib))))

(defun match-ps-file (filename ps-files)
  ""
  (find-if #'(lambda (file)
               (when (string= (namestring file) filename)
                 file))
           ps-files))

(defun compile-psruntime (libname)
  "Compile libs runtime forms"
  (let ((*package* (or (find-package (lookup-pkg libname)) *package*))
        (runtime (pslib-runtime (lookup-lib libname))))
    (ps:ps* runtime)))

(defun compile-pslib (libname)
  "Compile the whole lib returning javascript as a simple string"
  (apply #'concatenate 'string
         (compile-psruntime libname)
         (mapcar (curry #'compile-psfile libname)
                 (mapcar #'namestring (pslib-files (lookup-lib libname))))))

(defun compile-psfile (libname filename &key package)
  "Compile specific file from the lib, there is a special :package key which is used to set the compiling envornment, if it's not provided then package defined in lib will be used"
  (when-let ((ps-file (match-file libname filename)))
    (let ((*package* (or package
                         (find-package (lookup-pkg libname))
                         *package*)))
      (format t "~A~%" *package*)
      (ps:ps-compile-file (cl-fad:merge-pathnames-as-file (pslib-source-dir (lookup-lib libname))
                                                          ps-file)))))

(defun build-pslib-files (pslib)
  "Returnes a list of parenscript files in lib plus any
   parenscript file(s) found in source-directory"
  (nconc (pslib-files pslib)
         (files-with-ext (pslib-source-directory pslib) *ps-ext*)))

(defun string-ends-with (suffix str)
  "Returns T if string ends with suffix"
  (when-let ((found (search suffix str :from-end t)))
    (= (- (length str) (length suffix)) found)))

(defun files-with-ext (path ext &key (recursive nil))
  "Find all files in path filtered by extension ext"
  (flet ((filename-matches (filename)
           (string-ends-with ext (namestring filename))))
    (if recursive
        (let ((res))
          (cl-fad:walk-directory path
                                 (lambda (file)
                                   (when (filename-matches file)
                                     (pushnew file res))))
          res)
        (remove-if-not #'filename-matches (cl-fad:list-directory path)))))

;; components and asdf system

(defun system-ps-files (system)
  "Returns parenscript files found in asdf system
   Returns parenscript components in system, parenscript component
   is defined by paren-asdf package, and has a class of asdf:parenscript-file"
  (mapcar #'(lambda (component)
              (extract-subpath (asdf:system-source-directory system)
                               (asdf:component-pathname component)))
          (find-components (find-class 'asdf::parenscript-file) system)))

(defun find-components (class system)
  "Returns components by traversing system which are of a class"
  (let (res)
    (traverse-asdf-components #'(lambda (component)
                                  (when (eq (class-of component) class)
                                    (push component res)))
                              system)
    res))

(defun traverse-asdf-components (fun system)
  (labels ((traverse (lst)
             (cond ((null lst) nil)
                   ((eq (class-of (car lst)) (find-class 'asdf::module))
                    (progn
                      (traverse (asdf:component-children (car lst)))
                      (traverse (cdr lst))))
                   (t (progn
                        (funcall fun (car lst))
                        (traverse (cdr lst)))))))
    (traverse (asdf:component-children system))))
