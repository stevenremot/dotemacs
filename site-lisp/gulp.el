;;; gulp.el --- Gulp support fur Emacs

;;; Commentary:
;;

;;; Code:
(defgroup gulp
  '()
  "Gulp support."
  :group 'emacs)

(defcustom gulp-executable "gulp"
  "Path to gulp executable."
  :type 'string
  :group 'gulp)

(defcustom gulp-file-name "gulpfile.js"
  "File name of gulp file."
  :type 'string
  :group 'gulp)

(defcustom gulp-buffer-name "*gulp*"
  "Buffer name for gulp output buffer."
  :type 'string
  :group 'gulp)

(defun gulp-get-root (directory)
  "Return the parent directory containing the gulpfile.

Start to look in DIRECTORY.

Return nil of no gulp file has been found."
  (when (file-directory-p directory)
    (if (member gulp-file-name (directory-files directory))
        (file-name-as-directory directory)
      (let ((parent (file-name-directory (directory-file-name directory))))
        (when (not (string-equal directory parent))
          (gulp-get-root parent))))))

(defun gulp-open-buffer ()
  "Open a buffer for gulp output."
  (get-buffer-create gulp-buffer-name))

(defun gulp-start-task ()
  "Start a gulp task in a specified buffer.

TASK is a string specifying the task to start."
  (interactive)
  (let ((task (read-string "Enter a gulp task : "))
        (file-name (buffer-file-name)))
    (with-current-buffer (gulp-open-buffer)
      (setq default-directory (gulp-get-root (if (file-directory-p file-name)
                                                 file-name
                                                 (file-name-directory file-name))))
      (if default-directory
          (progn
            (start-process "gulp" (current-buffer) gulp-executable task)
            (gulp-mode))
        (setq default-directory "")
        (error "Cannot find gulp file")
        (kill-buffer)))))

(define-derived-mode gulp-mode compilation-mode "gulp"
  "Major mode for gulp execution.")


(provide 'gulp)

;;; gulp.el ends here
