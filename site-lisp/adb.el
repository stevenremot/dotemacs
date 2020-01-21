;;; adb.el --- Android tool helpers

;;; Commentary:
;;

;;; Code:
(defun avd-list-devices ()
  "Call avd to get a list of the devices.

TODO: Fix first element not catched."
  (let ((devices '()))
    (with-temp-buffer
     (call-process "avdmanager" nil t nil "list" "avd")
     (goto-char (point-min))

     (while (search-forward "Name: " nil t)
       (setq devices (push `(:name
			     ,(buffer-substring (point)
						(save-excursion
						  (end-of-line)
						  (point))))
			   devices))))
    devices)
  )

;;;###autoload
(defun avd-start-emulator (device-name)
  "Start an emulator.

DEVICE-NAME is the name of the virtual device to use."
  (interactive (list (completing-read "Device: "
				      (mapcar (lambda (device) (plist-get device :name))
					      (avd-list-devices)))))
  (async-shell-command (format "zsh -c \"cd $ANDROID_HOME/tools && emulator -avd %s\"" device-name) " *avd*"))

(provide 'adb)

;;; adb.el ends here
