(defpackage #:download-dependencies
  (:use :cl)
  (:export #:ensure-system
           #:*dependencies-home*))

(in-package #:download-dependencies)

(defvar *dependencies-home*)
(defvar *visited-dependencies* nil)

(defun ensure-system (system &key silent source-type source)
  (declare (type (or null keyword) source-type))
  (let (system-home)
    (if (and source source-type)
        (uiop:with-current-directory (*dependencies-home*)
          (let* ((system-find-output
                   (uiop:run-program (list "find" "." "-name"
                                           (format nil "~A.asd" (string system)))
                                     :output :string
                                     :error-output *error-output*))
                 (foundp (find #\newline system-find-output)))
            (if foundp
                (update system source-type source)
                (download system source-type source))
            (setq system-home (merge-pathnames (ensure-directory-name system)
                                               *dependencies-home*))))
        (let ((asdf-system (asdf:find-system system nil)))
          (cond (asdf-system
                 (unless silent
                   (format t "Found ~A at ~A"
                           system (asdf:system-source-file asdf-system)))
                 (setq system-home
                       (uiop:pathname-directory-pathname
                        (asdf:system-source-file asdf-system))))
                (t
                 (error "No ASDF system found with name ~A.~%Did you mean to supply SOURCE and SOURCE-TYPE?" system)))))
    (ensure-system-dependencies system-home :silent silent)))

(defun ensure-system-dependencies (system-home &key silent)
  (let* ((deps-source-file (merge-pathnames #P".dependencies" system-home)))
    (when (probe-file deps-source-file)
      (let ((*dependencies-home*
              (if (boundp '*dependencies-home*)
                  *dependencies-home*
                  (let ((home (merge-pathnames #P"dependencies/" system-home)))
                    (ensure-directories-exist home)
                    ;; (prompt-if-directory-not-empty home)
                    home)))
            (*visited-dependencies* nil))
        (unless silent
          (format t "~&Downloading dependencies in ~A~%" *dependencies-home*))
        (download-dependencies deps-source-file :silent silent)))))

(defun ensure-directory-name (name)
  (if (uiop:string-suffix-p name "/")
      name
      (uiop:strcat name "/")))

(defun download-dependencies (dependencies-source-file &key silent)
  (with-open-file (f dependencies-source-file)
    (loop :while (listen f)
          :do (destructuring-bind (directory-name source-type source &rest args) (read f)
                (let ((directory (merge-pathnames (ensure-directory-name directory-name)
                                                  *dependencies-home*)))
                  (unless (member directory *visited-dependencies* :test #'uiop:pathname-equal)
                    (pushnew directory *visited-dependencies* :test #'uiop:pathname-equal)
                    ;; (print (cons :visited *visited-dependencies*))
                    (if (or (member directory (uiop:subdirectories *dependencies-home*)
                                    :test #'uiop:pathname-equal)
                            (probe-file directory))
                        (progn
                          (unless silent
                            (format t "~A is already available. Updating...~%" directory-name))
                          (apply #'update directory-name source-type source args))
                        (progn
                          (unless silent
                            (format t "Downloading ~A~%" source))
                          (apply #'download directory-name source-type source args)))
                    (let ((deps-source-file (merge-pathnames #P".dependencies" directory)))
                      (when (probe-file deps-source-file)
                        (download-dependencies deps-source-file :silent silent)))))))))

(defgeneric download (directory-name source-type source &rest args))

(defmethod download (directory-name (type (eql :git)) source &rest args &key depth)
  "FIXME: ARGS are currently unused"
  (declare (ignore args))
  (uiop:with-current-directory (*dependencies-home*)
    (uiop:run-program (if depth
                          (list "git" "clone"
                                "--depth" (if (stringp depth)
                                              depth
                                              (write-to-string depth))
                                source directory-name)
                          (list "git" "clone"
                                source directory-name))
                      :output *standard-output*
                      :error-output *error-output*)))

(defgeneric update (directory-name source-type source &rest args))

(defmethod update (directory-name (type (eql :git)) source &rest args)
  "FIXME: ARGS are currently unused"
  (declare (ignore args))
  (uiop:with-current-directory (*dependencies-home*)
    (uiop:run-program (format nil "cd ~A && git pull '~A'" directory-name source)
                      :output *standard-output*
                      :error-output *error-output*)))
