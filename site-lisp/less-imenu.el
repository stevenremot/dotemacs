;;; less-imenu.el --- Imenu support for LESS files

;;; Commentary:
;;
(require 's)
;;; Code:

(defun less-imenu-go-to-next-opening-brace (bound)
  "Place the point to the next opening brace.

BOUND is the limit to search in.

Return t if it found a brace, nil otherwise."
  (when (search-forward "{" bound t)
    (goto-char (match-beginning 0))
    t))

(defun less-imenu-go-to-selector-beginning ()
  "Place the point to the beginning of the selector.

It can place the point some spacings before the selector."
  (if (re-search-backward
       (rx (or "}"
               "{"
               ";"
               "*/"
               (group line-start "//" (zero-or-more not-newline) line-end)))
       nil
       t)
      (goto-char (match-end 0))
    (goto-char (point-min))))

(defun less-imenu-find-brace-end ()
  "Find the end of a brace, balancing it."
  (save-excursion
    (forward-list)
    (point)))

(defun less-imenu-integrate-sub-selectors (parent bound)
  "Integrate all the selectors at the same level, in the curraent group.

PARENT si the parent selector.
BOUND is the point which marks the end of the group.
ENTRIES is the entry list.

Modify ENTRIES in-place."
  (let ((entries '())
        (entry nil))
    (while (setq entry (less-imenu-integrate-next-selector parent bound))
      (setq entries (append entries entry)))
    entries))

(defun less-imenu-integrate-next-selector (parent bound)
  "Integrate the next selector from current point to bound in entry list.

PARENT is the parent index entry.
BOUND is the maximal point to search selector.
ENTRIES is the entry list.

Modify ENTRIES in-place."
  (when (less-imenu-go-to-next-opening-brace bound)
    (let ((end (point))
          (internal-bound nil)
          (selector "")
          entries '())
      (less-imenu-go-to-selector-beginning)
      (setq selector (if (string= parent "")
                         (s-trim (buffer-substring (point) end))
                       (concat parent " " (s-trim (buffer-substring (point) end)))))
      (goto-char end)
      (add-to-list 'entries (cons selector (point)))
      (setq internal-bound (less-imenu-find-brace-end))
      (forward-char)
      (setq entries (append entries (less-imenu-integrate-sub-selectors selector internal-bound)))
      (goto-char internal-bound)
      entries)))

(defun less-imenu-create-index ()
  "Create Imenu tree for current Less buffer."
  (save-excursion
    (goto-char (point-min))
    (less-imenu-integrate-sub-selectors "" (point-max))))

(provide 'less-imenu)

;;; less-imenu.el ends here
