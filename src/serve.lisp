
(in-package :serve.paren)

(defparameter *libs* (make-hash-table :test #'equal))
(defparameter *ps-ext* "paren")
(defparameter *js-ext* "js")
(defparameter *minify-script* "/usr/bin/closure")

(defclass pslib ()
  ((name :initarg :name :reader pslib-name
         :documentation "")
   (system-source-dir :initarg :system-source-dir :reader pslib-system-dir
                      :documentation "Files source directory")
   (paren-source-dir :initarg :paren-source-dir
                     :documentation "")
   (runtime :initarg :runtime :reader pslib-runtime
            :documentation "Parenscript forms that are included
            directly.")
   (files :initarg :files :accessor pslib-files
          :documentation "")
   (package :initarg :package :reader pslib-package
            :documentation "Package in context of which parenscript will be compiled.")
   (match-fn :initarg :match-fn :reader pslib-match-fn))
  (:default-initargs :match-fn #'match-ps-file))

(defgeneric pslib-files (lib)
  (:documentation "Return the list of pslib files, if you want files
to be compiled in custom order you can overwrite this method."))

(defmethod pslib-files ((lib pslib))
  "Default order for files are 1. javascript files 2. paren-source-dir
files in reverse order 3. directly set via :files option"
  (slot-value lib 'files))

;; lookup lib data

(defun defined-lib-names ()
  "Get list of all lib names"
  (hash-table-keys *libs*))

(defun extract-subpath (path1 path2)
  "Extract path1 from path2"
  ;(break)
  (pathname (subseq (namestring path2) (length (namestring path1)))))

(defun relative-filename (lib filename)
  "Given filename with full pathname we try to build relative pathname
   accodring to system definition registry."
  (let* ((source-directory (pslib-system-dir lib))
         (fullname (or (and (cl-fad:pathname-relative-p filename)
                            (truename filename))
                       filename)))
    (extract-subpath source-directory fullname)))
    
    ;; (if (cl-fad:pathname-relative-p filename)
    ;;     filename
    ;;     (extract-subpath source-directory filename))))

(defun match-file (lib filename)
  "Try to match files in lib against name. Libname may have files
comming from different sources, and this function provides flexibility
on how to match those files with filename using match-fn found in the
lib. Default one is match-ps-file."
  (funcall (pslib-match-fn lib) (pslib-files lib) filename))

(defun match-ps-file (files filename)
  "Given list of files return first found that exactly matches
filename."
  (find-if #'(lambda (file)
               (when (string= (namestring file) filename)
                 file))
           files))

;; compile

(defun build-pslib-files (pslib)
  "Returnes a list of parenscript files in lib plus any parenscript
   file(s) found in source-directory."
  (nconc (pslib-files pslib)
         (files-with-ext (pslib-source-directory pslib) *ps-ext*)))

(defun string-ends-with (suffix str)
  "Returns T if string ends with suffix."
  (when-let ((found (search suffix str :from-end t)))
    (= (- (length str) (length suffix)) found)))

(defun files-with-ext (path ext &key (recursive nil))
  "Find all files in path filtered by extension ext."
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

(defmethod pslib-compile ((lib pslib))
  "Compile the whole lib returning javascript as a simple string."
  (apply #'concatenate 'string
         (compile-psruntime lib)
         (mapcar (lambda (filename)
                   (compile-psfile lib filename))
                 (mapcar #'namestring (pslib-files lib)))))

(defmethod pslib-compile-psfile ((lib pslib) filename)
  "Compile specific file from the lib, there is a special :package key
which is used to set the compiling envornment, if it's not provided
then package defined in lib will be used."
  (when-let ((ps-file (match-file lib filename)))
    (let* ((fullname (cl-fad:merge-pathnames-as-file (pslib-system-dir lib)
                                                     ps-file)))
      (if (parenscriptp fullname)
          (ps:ps-compile-file fullname)
          (read-file fullname)))))

;; urls

(defun concatenate-url (host path)
  (concatenate 'string host "/" path))

(defun files-urls (lib files)
  (mapcar (curry #'file-url lib) files))

(defmethod file-url ((lib pslib) filename)
  (concatenate 'string
               (pslib-name lib)
               ":"
               (namestring filename)))

(defmethod pslib-urls ((lib pslib))
  (files-urls lib (pslib-files lib)))

;; misc

(defun parenscriptp (filename)
  (string-ends-with *ps-ext* (namestring filename)))

(defun read-file (filename)
  (let* ((stream (open filename))
         (seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defun write-file (filename content)
  "Write string into file"
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :create)
    (format stream content)))

(defun js-minify (js-string)
  "Minify js string"
  js-string)
