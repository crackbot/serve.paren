
(restas:define-module #:serve.paren-restas
  (:use :cl :split-sequence :alexandria :serve.paren)
  (:export #:*parenscript-ending*
           #:*route-prefix*))

(in-package #:serve.paren-restas)

(defvar *route* "*path")
(defvar *parenscript-ending* "paren")

(defun join (list &optional (delimiter #\&))
  (with-output-to-string (stream)
    (join-to-stream stream list delimiter)))

(defun join-to-stream (stream list &optional (delimiter #\&))
  (destructuring-bind (&optional first &rest rest) list
    (when first
      (write-string (string first) stream)
      (when rest
        (write-char delimiter stream)
        (join-to-stream stream rest delimiter)))))

(restas:define-route serve-route ("js/*path" :method :get)
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf (hunchentoot:content-type*) "text/javascript")
  (destructuring-bind (libname filename) (split-sequence:split-sequence #\: (join path #\/))
    (if (string= filename "runtime.paren")
        (compile-psruntime libname)
        (if-let ((compiled-js (compile-psfile libname filename)))
          compiled-js
          hunchentoot:+http-not-found+))))

;; (defun files-urls (pkg)
;;   (let* ((lib (lookup-lib pkg))
;;          (files (get-lib-files lib)))
;;     (mapcar (curry #'build-url-for-file pkg) files)))

;; (defun build-url-for-file (pkg file)
;;   (concatenate 'string
;;                (string-downcase (symbol-name pkg))
;;                ":"
;;                (namestring (asdf:component-relative-pathname file))))

;; (defun build-specific-urls (pkg given-files)
;;   (let* ((lib (lookup-lib pkg))
;;          (files (get-lib-files lib)))
;;     (mapcar (curry #'build-url-for-file pkg)
;;             (remove nil (mapcar #'(lambda (given-file)
;;                                     (match-ps-file given-file files))
;;                                 given-files)))))


  ;; (let* ((relative-path (parse-native-namestring (format nil "~{~A~^/~}" path)))
  ;;        (path (merge-pathnames relative-path
  ;;                               *directory*)))
     
   
  ;;   (cond
  ;;     ((or (find :up (pathname-directory relative-path))
  ;;          (find :absolute (pathname-directory relative-path)))
  ;;      hunchentoot:+http-bad-request+)
  ;;     ((ignore-pathname-p path)
  ;;      hunchentoot:+http-not-found+)
  ;;     ((and (fad:directory-pathname-p path)
  ;;           (fad:directory-exists-p path))
  ;;      (or (iter (for index in *directory-index-files*)
  ;;                (let ((index-path (merge-pathnames index path)))
  ;;                  (finding index-path
  ;;                           such-that (fad:file-exists-p index-path))))
  ;;          (if *autoindex*
  ;;              (directory-autoindex-info path relative-path)
  ;;              hunchentoot:+http-not-found+)))
  ;;     ((not (fad:file-exists-p path))
  ;;      hunchentoot:+http-not-found+)
  ;;     (t path))))

;; (defun path-last-name (path)
;;   "File or directory name"
;;   (if (fad:directory-pathname-p path)
;;       (car (last (cdr (pathname-directory path))))
;;       (let ((name (pathname-name path))
;;             (type (pathname-type path)))
;;         (if type
;;             (format nil "~A.~A" name type)
;;             name))))

;; (defparameter *byte-units* '("kB" "MB" "GB" "TB" "PB" "EB" "ZB" "YB")
;;   "Symbols for show size of files in human readable format")

;; (defun format-size (bytes)
;;   "Convert number of bytes to human readable format (e.g. 1K 234M 2G)"
;;   (let* ((unit (if (> bytes 0)
;;                    (floor (log bytes 1024))
;;                    bytes))
;;          (symbol (if (> unit 0)
;;                      (nth (1- unit)
;;                           *byte-units*))))
;;     (if symbol
;;         (format nil
;;                 "~,1F ~A"
;;                 (/ bytes (expt 1024 unit))
;;                 symbol)
;;         (format nil "~A B" bytes))))

;; (defun format-last-modified (timestamp)
;;   (local-time:format-timestring
;;    nil
;;    timestamp
;;    :format '((:day 2) #\- :short-month #\- :year #\Space (:hour 2) #\: (:min 2))))

;; #+(and sbcl unix)
;; (defun pathname-info (path)
;;   (let ((stat (sb-posix:stat path))
;;         (isdir (fad:directory-pathname-p path)))
;;     (list :mime-type (if (not isdir) (hunchentoot:mime-type path))
;;           :name (path-last-name path)
;;           :last-modified (format-last-modified (local-time:unix-to-timestamp (sb-posix:stat-mtime stat)))
;;           :size (if (not isdir) (format-size (sb-posix:stat-size stat))))))

;; #-(and sbcl unix)
;; (defun pathname-info (path)
;;   (let ((isdir (fad:directory-pathname-p path)))
;;     (list :mime-type (if (not isdir) (hunchentoot:mime-type path))
;;           :name (path-last-name path)
;;           :last-modified (format-last-modified (local-time:universal-to-timestamp (file-write-date path)))
;;           :size (if (not isdir) (ignore-errors
;;                                   (format-size
;;                                    (with-open-file (s path) (file-length s))))))))

;; (defun hidden-pathname-p (path)
;;   (if (not (equal path
;;                   (make-pathname :directory (list :absolute))))
;;       (char= (char (path-last-name path) 0)
;;              #\.)))

;; (defun ignore-pathname-p (path)
;;   (if *ignore-pathname-p*
;;       (funcall *ignore-pathname-p* path)
;;       (hidden-pathname-p path)))

;; (defun directory-autoindex-info (path rpath)
;;   "Info on directory for autoindex"
;;   (labels ((sort-by-name (seq)
;;              (sort seq
;;                    #'(lambda (a b)
;;                        (string< (getf a :name)
;;                                 (getf b :name))))))
;;     (let (directories files)
;;       (iter (for item in (fad:list-directory (merge-pathnames path *directory*)))
;;             (unless (ignore-pathname-p item)
;;               (if (fad:directory-pathname-p item)
;;                   (push item directories)
;;                   (push item files))))
;;       (list :title (format nil "Index of /~A" rpath)
;;             :curdir (or (car (last (cdr (pathname-directory rpath))))
;;                         "/")
;;             :parents (if (not (equal path *directory*))
;;                          (cons (list :href (restas:genurl 'route
;;                                                           :path '("")
;;                                                           ))
;;                                (iter (for item in (butlast (cdr (pathname-directory rpath))))
;;                                      (collect item into curpath)
;;                                      (collect (list :href (restas:genurl 'route
;;                                                                          :path (append curpath
;;                                                                                        '("")))
;;                                                     :name item)))))
;;             :directories (sort-by-name (iter (for item in directories)
;;                                              (collect (list* :href (restas:genurl 'route
;;                                                                                   :path (append (cdr (pathname-directory rpath))
;;                                                                                                 (list (path-last-name item) "")))
;;                                                              (pathname-info item)))))
;;             :files (sort-by-name (iter (for item in files)
;;                                        (collect (list* :href (restas:genurl 'route
;;                                                                             :path (append (cdr (pathname-directory rpath))
;;                                                                                           (list (path-last-name item))))
;;                                                        (pathname-info item)))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

