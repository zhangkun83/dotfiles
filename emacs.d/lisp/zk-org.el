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

(defun zk-org-valid-tag-char-p (c)
  (or (and (>= c ?a) (<= c ?z))
      (and (>= c ?A) (<= c ?A))
      (and (>= c ?0) (<= c ?9))
      (= c ?_)
      (= c ?@)
      ;; "#" is not listed as a valid char in the doc, but I'm using
      ;; it just fine.
      (= c ?#)))

(defun zk-org-mark-heading-content ()
  "Mark the content of the current heading, excluding subtrees.
The marked region starts after the current heading and ends before the
next heading of the same or higher level.  The point is at the beginning
of the region, while the mark is at the end."
  (org-back-to-heading)
  (let* ((beg (progn (forward-line) (point)))
         (end (or (outline-next-heading) (point-max))))
    (set-mark end)
    (goto-char beg)))

(defun zk-org-get-current-heading-link ()
  "Get the link to the current Org heading without affecting org-stored-links.

This function prioritizes link types in the following order:
1. CUSTOM_ID
2. ID
3. Heading title (fuzzy link)

It returns the full link string, including the filename."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-back-to-heading)
      (let* ((element (org-element-at-point))
             (file (buffer-file-name)))
        ;; First, ensure we are actually on a headline.
        (when (and element (eq (org-element-type element) 'headline))
          (let* (;; Get all relevant properties from the headline element.
                 (custom-id (org-element-property :CUSTOM_ID element))
                 (id (org-element-property :ID element))
                 (title (org-element-property :title element)))
            (cond
             ;; 1. If CUSTOM_ID exists, use it. Format is [[file::#custom-id]]
             (custom-id (format "file:%s::#%s" file custom-id))
             ;; 2. If ID exists, use it. Format is [[file::#id-uuid]]
             (id (format "file:%s::#%s" file id))
             ;; 3. Otherwise, fall back to the heading title. Format is [[file::*Heading Title]]
             (title (format "file:%s::*%s" file title))
             ;; 4. Fallback if something is wrong.
             (t nil))))))))

(advice-add 'org-agenda-switch-to :around #'zk-org-push-mark-ring-advice)
(advice-add 'org-next-link :around #'zk-push-mark-ring-advice)
(advice-add 'org-next-visible-heading :around #'zk-push-mark-ring-advice)
(advice-add 'outline-up-heading :around #'zk-push-mark-ring-advice)

(provide 'zk-org)
