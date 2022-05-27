(defun my-mermaid-preview ()
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer))))
    (shell-command
     (format
      "mmdc -i %s -o /tmp/test.md && cmark-gfm /tmp/test.md --extension table --validate-utf8 > /tmp/test.html"
      file-name))
    (xwidget-webkit-browse-url "/tmp/test.html")
    (xwidget-webkit-reload)))
