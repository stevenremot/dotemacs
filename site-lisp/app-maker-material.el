;;; app-maker-material.el --- Material UI elements for app-maker

;;; Commentary:
;;

(require 'app-maker)

;;; Code:

;; Basic layout

(defun amui/page (props &rest children)
  "Root component of a Material UI page.

PROPS can have `:title'

CHILDREN will be included in the body."
  `(html
    ()

    (head
     ()
     (title () ,(plist-get props :title))
     (meta (:name "viewport" :content "width=device-width"))
     (link (:rel "manifest" :href ,(concat "/" am/app-name "/manifest.json")))
     (link (:href "//cdn.muicss.com/mui-0.10.3/css/mui.min.css" :rel "stylesheet" :type "text/css"))
     (script (:src "//cdn.muicss.com/mui-0.10.3/js/mui.min.js")))

    (body
     ()
     ,@children)))

(defun amui/flex (props &rest children)
  "A flex row or column.

PROPS can have:
 - `:dir' - \"row\" or \"column\"
 - `:gap' - A gap value
 - `:align' - Value for align-items

CHILDREN is the layout content."
  (let ((direction (or (plist-get props :dir) "row"))
	(gap (or (plist-get props :gap) "8px"))
	(align (or (plist-get props :align) "initial")))
    `(div
      ,(list
	:style (format "
display: flex;
flex-direction: %s;
align-items: %s;
gap: %s;" direction align gap))
      ,@children)))

;; Text

(defun amui/text (props &rest children)
  "Applying correct style on text.

PROPS can have:
 - `:style' - muicss typo values

CHILDREN is the DOM content."
  `(span
    (:class ,(format "mui--text-%s" (plist-get props :style)))
    ,@children))

;; Buttons

(defun amui/button (props &rest children)
  "Create a flat button.

PROPS can have:
- `:style' (nil, \"primary\", \"danger\", \"absent\")
- `:on-click': function

CHILDREN is the button text / DOM content."
  (let* ((style (plist-get props :style))
	 (style-class (if style (format "mui-btn--%s" style) "")))
    `(a
      (:class ,(format "mui-btn %s" style-class)
       :href ,(plist-get props :on-click))

      ,@children)))

(defun amui/button-flat (props &rest children)
  "Create a flat button.

PROPS can have:
- `:style' (nil, \"primary\", \"danger\", \"absent\")
- `:on-click': function

CHILDREN is the button text / DOM content."
  (let* ((style (plist-get props :style))
	 (style-class (if style (format "mui-btn--%s" style) "")))
    `(a
      (:class ,(format "mui-btn mui-btn--flat %s" style-class)
       :href ,(plist-get props :on-click))

      ,@children)))

(provide 'app-maker-material)

;;; app-maker-material.el ends here
