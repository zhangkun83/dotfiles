(require 'org)
(require 'zk-clipboard)

(require 'ox)  ; defines org-export-with-drawers
(defun zk-org-export-html-to-clipboard (arg)
  "Export the whole file or the active region as HTML to the
clipboard.  If called with prefix argument, also export LOGBOOK
 drawers."
  (interactive "P")
  (let* ((org-export-with-drawers (if arg t org-export-with-drawers))
         (org-export-show-temporary-export-buffer nil)
         (buffer (org-html-export-as-html)))
    (with-current-buffer buffer
      (zk-clipboard-copy t))))

(defun zk-org-extract-scheduled-timestamp-string (element)
  "Return the raw SCHEDULED timestamp string for the given org
element.  If there is no SCHEDULED timestamp, return nil."
  (let ((timestamp (org-element-property :scheduled element)))
    (if timestamp
        (plist-get (car (cdr timestamp)) ':raw-value)
      nil)))

(defun zk-org-scheduled-for-today-p (element)
  "Return t if the given element has a SCHEDULED timestamp that
 is on today."
  (let ((scheduled-timestamp (zk-org-extract-scheduled-timestamp-string element)))
    (if scheduled-timestamp (= 0 (org-time-stamp-to-now scheduled-timestamp))
      nil)))

(provide 'zk-org)
