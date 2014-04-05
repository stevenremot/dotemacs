;;; gulp.el --- Gulp support fur Emacs

;;; Commentary:
;; TODO Make gulp restart work

;;; Code:
(defgroup gulp
  '()
  "Gulp support."
  :group 'tools)

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

(defvar-local gulp-task ""
  "Gulp task run in the current buffer.")

(defvar-local gulp-directory ""
  "Gulp directory for current gulp task.")

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

(defun gulp-process-sentinel (process event)
  "Watch the modifications for gulp PROCESS.

EVENT is the proces' status change."
  (when (string-match-p "exited abnormally" event)
    (message (propertize "Gulp process stopped unexpectedly" 'face 'error))))

(defun gulp-create-process-for-task (file-name task)
  "Launch the gulp process in FILE-NAME for TASK."
  (setq default-directory (gulp-get-root (if (file-directory-p file-name)
                                             file-name
                                           (file-name-directory file-name)))
        gulp-task task
        gulp-directory default-directory)
  (if default-directory
      (let ((process (start-process "gulp" (current-buffer) gulp-executable task)))
        (set-process-sentinel process 'gulp-process-sentinel)
        (gulp-mode))
    (setq default-directory "")
    (error "Cannot find gulp file")
    (kill-buffer)))

;;;###autoload
(defun gulp-start-task ()
  "Start a gulp task in a specified buffer.

TASK is a string specifying the task to start."
  (interactive)
  (let ((task (read-string "Enter a gulp task : "))
        (file-name (buffer-file-name)))
    (with-current-buffer (gulp-open-buffer)
      (gulp-create-process-for-task file-name task))))

(defun gulp-restart-task ()
  "Restart the gulp task run in the current buffer."
  (interactive)
  (with-current-buffer (gulp-open-buffer)
    (gulp-create-process-for-task gulp-directory gulp-task)))

;;;;;;;
;; Mode
;;;;;;;

(defvar gulp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'gulp-restart-task)
    map)
  "Keymap for `gulp-mode'.")

(define-derived-mode gulp-mode compilation-mode "gulp"
  "Major mode for gulp execution.
\\{gulp-mode-map}")


(provide 'gulp)

;;; gulp.el ends here
