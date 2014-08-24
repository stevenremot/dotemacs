;;; ede-php-root.el --- Simple EDE PHP Project

;;; Commentary:
;;
;; Simple PHP project for EDE.  Inspired by `ede-cpp-root-project'.
(require 'ede)
;;; Code:

(defvar ede-php-root-project-list nil
  "List of projects created by otpion `ede-php-root-project'.")

(defun ede-php-root-file-existing (dir)
  "Find a php-root project in the list of php-root projects.
DIR is the drectory to search from."
  (let ((projs ede-php-root-project-list)
        (ans nil))
    (while (and projs (not ans))
      (let ((root (ede-project-root-directory (car projs))))
        (when (string-match (concat "^" (regexp-quote root)) dir)
          (setq ans (car projs))))
      (setq projs (cdr projs)))
    ans))

(defun ede-php-root-project-file-for-dir (&optional dir)
  "Return a full file name to the project file stored in DIR."
  (let ((proj (ede-php-root-file-existing dir)))
    (when proj (oref proj :file))))

;;;###autoload
(defun ede-php-root-project-root (&optional dir)
  "Get the root directory for DIR."
  (let ((projfile (ede-php-root-project-file-for-dir
                   (or dir default-directory))))
    (when projfile
      (file-name-directory projfile))))

(defun ede-php-root-load (dir &optional rootproj)
  "Return a PHP root object if you created one.
Return nil if there isn't one.
DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (ede-php-root-file-existing dir))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "php-root"
                       :name "PHP ROOT"
                       :file 'ede-php-root
                       :proj-file 'ede-php-root-project-file-for-dir
                       :proj-root 'ede-php-root-project-root
                       :load-type 'ede-php-root-load
                       :class-sym 'ede-php-root-project
                       :new-p nil
                       :safe-p t)
 'unique)

;;;;
;;;; Class loaders
;;;;
;;;; In modern PHP, there is no thing like "#include" or "import".
;;;; The unknown classes are loaded at runtime using a custom loader.
;;;;
;;;; For example, with PSR-2 convention, to find the class \Bar\Foo
;;;; one have to search each include path to find the file Bar/Foo.php.

(defclass ede-php-root-class-loader ()
  ()
  "Base class for finding the file in with some class is defined."
  :abstract t)

(defmethod ede-php-root-find-class-def-file ((this ede-php-root-class-loader)
                                             class-name)
  "Find the file in which CLASS-NAME is defined.

CLASS-NAME must be the full name of the class, with all its parent namespaces."
  (error "Method `ede-php-root-find-class-def-file' must be overriden"))

;;;###autoload
(defclass ede-php-root-psr2-class-loader (ede-php-root-class-loader)
  ((include-paths :initarg :include-paths
                  :initform ()
                  :documentation
                  "A list of the paths in which classes are defined.
The paths must be relative to the project root."))
  "Class loader for PSR-2 convention.")

(defmethod ede-php-root-find-class-def-file ((this ede-php-root-psr2-class-loader)
                                             class-name)
  "Find the file in which CLASS-NAME is defined.

Return nil if no file has been found."
  (let* ((namelist (split-string class-name (regexp-quote "\\") t))
         (relative-path (concat (mapconcat 'identity namelist "/") ".php"))
         (project-root (ede-project-root-directory (ede-current-project)))
         (include-paths (oref this include-paths))
         class-def-file)
    (while (and include-paths (not class-def-file))
      (let* ((include-path (car include-paths))
             (candidate-file (expand-file-name relative-path
                                               (expand-file-name include-path
                                                                 project-root))))
        (when (file-regular-p candidate-file)
          (setq class-def-file candidate-file))
        (setq include-paths (cdr include-paths))))
    class-def-file))

(defclass ede-php-root-target (ede-target)
  ((project :initform nil
            :initarg :project))
  "EDE php-root project target.")

;;;###autoload
(defclass ede-php-root-project (ede-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-php-root-project-list)
   (class-loader :initarg :class-loader
                 :type ede-php-root-class-loader
                 :documentation "The project's class loader.")))

(defmethod initialize-instance ((this ede-php-root-project) &rest fields)
  "Make sure the :file is fully expanded."
  (call-next-method)
  (let ((f (expand-file-name (oref this :file))))
    ;; Remove any previous entries from the main list.
    (let ((old (eieio-instance-tracker-find (file-name-directory f)
                                            :directory
                                            'ede-php-root-project-list)))
      (when (and old (not (eq old this)))
        (delete-instance old)))
    ;; Basic initialization.
    (when (or (not (file-exists-p f))
              (file-directory-p f))
      (delete-instance this)
      (error ":file for ede-php-root-project must be a file"))
    (oset this :file f)
    (oset this :directory (file-name-directory f))
    (ede-project-directory-remove-hash (file-name-directory f))
    (ede-add-project-to-global-list this)
    (unless (slot-boundp this 'targets)
      (oset this :targets nil))))

(defmethod ede-find-subproject-for-directory ((proj ede-php-root-project) dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

(defmethod ede-find-target ((proj ede-php-root-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((targets (oref proj targets))
         (dir default-directory)
         (ans (object-assoc dir :path targets)))
    (when (not ans)
      (setq ans (ede-php-root-target dir
                                     :name (file-name-nondirectory
                                            (directory-file-name dir))
                                     :path dir
                                     :source nil
                                     :project proj))
      (object-add-to-list proj :targets ans))
    ans))

(defmethod ede-project-root ((this ede-php-root-project))
  "Return my root."
  this)

(defmethod ede-project-root-directory ((this ede-php-root-project))
  "Return my root."
  (file-name-directory (oref this file)))

(defmethod ede-php-root-find-class-def-file ((this ede-php-root-project) class-name)
  "Find the file in which CLASS-NAME is defined.

CLASS-NAME must be the full name of the class, with all its parent namespaces."
  (ede-php-root-find-class-def-file (oref this class-loader) class-name))

(provide 'ede-php-root)

;;; ede-php-root.el ends here
