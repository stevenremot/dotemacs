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
(defun adb-list-processes ()
  "List all processes running on the device."
  (interactive)
  (async-shell-command "adb shell ps -A" "*adb ps*")
  (display-buffer "*adb ps*"))

;;;###autoload
(defun adb-logcat-tail ()
  "Run logcat."
  (interactive)
  (async-shell-command "adb logcat -T 20" "*adb logcat*")
  (display-buffer "*adb-logcat*"))

;;;###autoload
(defun adb-logcat-pid (pid)
  "Run logcat, filtering for PID."
  (interactive (list (thing-at-point 'number)))
  (async-shell-command (format "adb logcat --pid %d" pid) "*adb logcat*")
  (display-buffer "*adb-logcat*"))


;;;###autoload
(defun avd-start-emulator (device-name)
  "Start an emulator.

DEVICE-NAME is the name of the virtual device to use."
  (interactive (list (completing-read "Device: "
				      (mapcar (lambda (device) (plist-get device :name))
					      (avd-list-devices)))))
  (async-shell-command (format "cd $ANDROID_HOME/tools && emulator -avd %s" device-name) " *avd*"))

(provide 'adb)

;;; adb.el ends here
