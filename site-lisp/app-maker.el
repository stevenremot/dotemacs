;;; app-maker.el --- Basic web apps maker -*- lexical-binding: t -*-
;; Packages-Requires: (simple-httpd)

;;; Commentary:
;;
;;; Code:
(require 'cl-lib)
(require 'simple-httpd)

;; Variables

(defvar am/current-actions nil)
(defvar am/current-session-uuid nil)
(defvar am/app-name nil)

;; Helpers

(defun am/uuid-create ()
  "Generate a new UUID."
  (with-temp-buffer
    (cond
     ((executable-find "uuidgen") (call-process "uuidgen" nil t))
     ((executable-find "uuid") (call-process "uuid" nil t))
     (t (error "No executable found for uuid generation")))

    (string-trim-right (buffer-substring-no-properties (point-min) (point-max)))))

;; HTML Rendering

(cl-defgeneric am/render (_)
  "Return an HTML spec.")

(defun am/format-attr (attr)
  "Format ATTR as string.

Format functions using a link."
  (if (functionp attr)
      (let ((action-uuid (am/uuid-create)))
       (puthash action-uuid attr am/current-actions)
       (format "?session=%s&action=%s" am/current-session-uuid action-uuid))
    attr)
  )

(defun am/render-html-attrs (attrs)
  "Render ATTRS into an HTML list of attributes."
  (cl-loop for i from 0 to (1- (length attrs)) by 2
	     collect (format " %s=\"%s\""
			     (substring (symbol-name (nth i attrs)) 1)
			     (am/format-attr (nth (1+ i) attrs)))
	     into string-pairs
	     finally return (mapconcat #'identity string-pairs "")))

(defun am/render-html (spec)
  "Render SPEC as an HTML string."
  (pcase spec
    ((pred numberp) (number-to-string spec))
    ((pred stringp) spec)
    ((pred listp)
     (let ((tag (car spec))
	     (attrs (cadr spec))
	     (children (cddr spec)))
	 (format "<%s%s>%s</%s>"
		 tag
		 (am/render-html-attrs attrs)
		 (mapconcat #'am/render-html children "")
		 tag)))
    (t (am/render-html (am/render spec)))))

;; Session management

(defun am/session-put (sessions uuid value)
  "Add an entry to the SESSIONS at UUID for VALUE."
  (puthash uuid value sessions))

(defun am/session-get (sessions uuid default)
  "Return, if it exists, the session in SESSIONS at UUID.

Calls DEFAULT when no entry has been found."
  (let ((value (gethash uuid sessions)))
    (if value
	value
      (setq value (funcall default))
      (am/session-put sessions uuid value)
      value)))

;; App life cycle

(defun am/handle-call (name query sessions actions body)
  "Return the HTML for an HTTP call by executing BODY."
  (let* ((session-uuid (or (cadr (assoc "session" query)) (am/uuid-create)))
	 (action-uuid (cadr (assoc "action" query)))
	 (state (am/session-get sessions session-uuid #'(lambda () (eval body))))
	 (action (if action-uuid (gethash action-uuid actions) nil))
	 (am/current-actions actions)
	 (am/current-session-uuid session-uuid)
	 (am/app-name name))
    (when action
      (funcall action))
    (clrhash actions)
    (insert (am/render-html state))))

(defmacro am/defapp (name &rest body)
  "Define a handler for a basic app with NAME, ARGS and BODY."
  (declare (indent defun))
  `(let ((sessions (make-hash-table :test #'equal))
	 (actions (make-hash-table :test #'equal)))
     (defservlet ,name text/html (path query)
       (am/handle-call ,(symbol-name name) query sessions actions (quote (progn ,@body))))

     (defservlet ,(intern (format "%s/manifest.json" (symbol-name name))) application/manifest+json ()
       (insert ,(json-encode-plist (list
				    'name (symbol-name name)
				    'short_name (symbol-name name)
				    'start_url "."
				    'display "standalone"))))))

(provide 'app-maker)

;;; app-maker.el ends here

;; Local Variables:
;; after-save-hook: (lambda () (progn (eval-buffer) (ert-run-tests-interactively t)))
;; End:
