;;; p5js.el --- Tools to help in creating p5js sketches -*- lexical-binding: t -*-

;;; Commentary:
;;
(require 'simple-httpd)
;;; Code:

(defvar p5js-html-template "
<html>
  <head>
    <script src=\"https://cdn.jsdelivr.net/npm/p5@1.1.9/lib/p5.js\"></script>
    <script src=\"/p5js/sketch.js\"></script>
  </head>
  <body>
    <main>
    </main>
    <script>

     function exportCanvas(evt) {
         const uri = document.querySelector('canvas')
     	  .toDataURL()
         const link = document.createElement('img')
         link.src = uri
         document.body.appendChild(link)
     }

     const button = document.createElement('button')

     button.onclick = exportCanvas
     button.textContent = 'Export'

     window.addEventListener(
         'DOMContentLoaded',
         () => document.body.appendChild(button))
    </script>
  </body>
</html>
" "HTML template fdor the p5js index page.")

(defun p5js-start-for-buffer (buffer)
  "Start the p5js environment for the current BUFFER."
  (interactive "b")

  (defservlet p5js text/html ()
    (insert p5js-html-template))

  (defservlet p5js/sketch.js text/javascript ()
    (insert (with-current-buffer buffer
	      (buffer-substring-no-properties (point-min) (point-max)))))

  (httpd-start))

(defun p5js-open-page ()
  "Open the page with p5js result."
  (interactive)
  (let ((url (format "http://localhost:%i/p5js" httpd-port)))
    (if (executable-find "termux-open")
	(async-shell-command (format "termux-open %s" url))
      (browse-url url))))

(provide 'p5js)

;;; p5js.el ends here
