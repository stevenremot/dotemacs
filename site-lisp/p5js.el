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
  (browse-url (format "http://localhost:%i/p5js" httpd-port)))

(provide 'p5js)

;;; p5js.el ends here
