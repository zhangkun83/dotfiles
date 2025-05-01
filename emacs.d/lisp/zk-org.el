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

(defun zk-org-link-at-point-p ()
  "Return t if the point is on a link.  nil otherwise."
   (let* ((context (org-element-context))
          (type (org-element-type context)))
     (eq type 'link)))

(defun zk-org-open-next-link (arg)
  "If the point is on a link, open it.  Otherwise, move point to the
next link and open it.  If the prefix arg is non-nil, move
backward."
  (interactive "P")
  (if (zk-org-link-at-point-p)
      (org-open-at-point)
    (org-next-link arg)
    (org-open-at-point)))

(defun zk-org-push-mark-ring-advice (orig-fun &rest args)
  "Put this advice around any function to push the original
  buffer and point to the org mark ring if the function changes
 the point"
  (let ((pos (point))
        (buffer (current-buffer)))
    (apply orig-fun args)
    (unless (and (equal buffer (current-buffer))
                 (equal pos (point)))
      (org-mark-ring-push pos buffer))))

(advice-add 'org-agenda-switch-to :around #'zk-org-push-mark-ring-advice)

(provide 'zk-org)
