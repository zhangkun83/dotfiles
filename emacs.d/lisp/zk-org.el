(require 'org)
(require 'zk-clipboard)
(require 'zk)

(require 'ox)  ; defines org-export-with-drawers

(defun zk-org-init-fonts ()
  (when (display-graphic-p)
    (setq org-hide-emphasis-markers t)

    ;; Default to proportional font in org-mode and org-agenda-mode
    (add-hook 'org-mode-hook 'zk-use-proportional-font-for-current-buffer)
    (add-hook 'org-agenda-mode-hook 'zk-use-proportional-font-for-current-buffer)

    ;; Remove the boldness from several elements because they don't look
    ;; good with proportional fonts.
    (set-face-attribute 'org-agenda-calendar-event nil :weight 'regular)
    (set-face-attribute 'org-scheduled-today nil :weight 'regular)

    ;; Keep keywords and code on default (monospace) font
    (dolist (face '(org-code
                    org-block
                    org-block-begin-line
                    org-block-end-line
                    org-meta-line
                    org-property-value
                    org-date
                    org-table
                    org-checkbox))
      (set-face-attribute face nil :font zk-font-family))
    ;; While these faces are already bold, they will mysteriously lose
    ;; their boldness in new frames.  This will fix them.
    (dolist (face '(org-todo
                    org-done
                    org-drawer
                    org-special-keyword))
      (set-face-attribute face nil :font zk-font-family :weight 'bold))))


(defun zk-org-process-exported-html (src-file &optional exported-ids-ht)
  "Process the exported HTML in the current buffer.

SRC-FILE is the file name of the source org file.

EXPORTED-IDS-HT is a hashtable of the CUSTOM_IDs of all exported
headings.  If nil, assume all CUSTOM_IDs of SRC-FILE are exported."
  ;; org-mode adds nbsp in headings between components, so timestamps
  ;; in the headings are usually broken at the space between the date
  ;; and the day of the week.  Add breakable spaces before and after
  ;; timestamps, and put them in <nobr>, so that the timestamp can
  ;; stay as a whole.
  (replace-regexp-in-region
   "<span class=\"timestamp\">\\([^<]+\\)</span>"
   "<span class=\"timestamp\"> <nobr>\\1</nobr> </span>"
   (point-min) (point-max))

  ;; Change timestamp's style
  (replace-regexp-in-region
   "\\.timestamp +{[^}]*}"
   ".timestamp { color: blue; font-family: monospace; }"
   (point-min) (point-max))

  ;; Add <br> to the end of RE: lines, so that they don't merge into
  ;; one line.
  (goto-char 0)
  (while (search-forward-regexp "^RE: .*<a href=.*</a>$" (point-max) t)
    (insert "<br>"))

  ;; Check all hyper links.  If it points to the same file, and the
  ;; destination ID is included in the export, remove the file name
  ;; and keep the anchor.
  (goto-char 0)
  (let ((output-html-file-name
         (concat
          (file-name-sans-extension (file-name-nondirectory src-file)) ".html")))
    (while (search-forward-regexp "<a href=\"\\([^/\"#]+\\)#\\([^\"]+\\)\">" nil t)
      (let ((file-name (match-string-no-properties 1))
            (anchor (match-string-no-properties 2)))
        (when
            (and
             (string-equal file-name output-html-file-name)
             (or (not exported-ids-ht)
                 (gethash anchor exported-ids-ht)))
          (delete-region (match-beginning 1) (match-end 1))))))
  ;; If a link still contains file name at this point, it's pointing
  ;; to an unreachable destination.  Remove those links.
  (replace-regexp-in-region
   "<a href=\"\\([^/\"]+\\.html#[^\"]+\\)\">" 
   "<a style=\"background-color: red;\" title=\"Unreachable link: \\1\">" 
   (point-min) (point-max)))

(defun zk-org-export-html-to-clipboard (arg)
  "Export the whole file or the active region as HTML to the
clipboard.  If called with prefix argument, also export LOGBOOK
 drawers."
  (interactive "P")
  (let* ((org-export-with-drawers (if arg t org-export-with-drawers))
         (org-export-show-temporary-export-buffer nil)
         (whole-file-p (not (use-region-p)))
         (org-export-with-toc whole-file-p)
         (org-export-with-section-numbers whole-file-p)
         (src-file-name (buffer-file-name))
         (buffer (org-html-export-as-html))
         (exported-ids-ht nil))
    (when (not whole-file-p)
      (save-mark-and-excursion
        (setq exported-ids-ht (make-hash-table :test 'equal))
        (let ((start (region-beginning))
              (end (region-end)))
          (goto-char start)
          (while (re-search-forward "^:CUSTOM_ID: +\\(.+\\)$" end t)
            (puthash (match-string-no-properties 1) t exported-ids-ht)))))
    (with-current-buffer buffer
      (zk-org-process-exported-html src-file-name exported-ids-ht)
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

(defun zk-org-get-link-at-point ()
  "Return the link at point."
  (org-element-property :raw-link (org-element-context)))

(defun zk-org-open-next-link (&optional arg)
  "If the point is on a link, open it.  Otherwise, move point to the
next link and open it.  If the prefix arg is non-nil, move
backward."
  (interactive "P")
  (unless (eq major-mode 'org-mode)
    (user-error "Not in org-mode."))
  (org-open-link-from-string
   (if (zk-org-link-at-point-p)
       (zk-org-get-link-at-point)
     (org-next-link arg)
     (zk-org-get-link-at-point))))

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
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (beginning-of-line)
  (forward-line)
  (let* ((beg (point))
         (end (if (org-at-heading-p)
                   ;; A corner case where the heading has no content,
                   ;; thus forward-line just moved the point to the
                   ;; next heading.
                   (point)
                (or (outline-next-heading) (point-max)))))
    (set-mark end)
    (goto-char beg)))

(defun zk-org-expand-drawer-at-point ()
  "Expand the Org mode drawer at point, even from the middle.  Do nothing
if not in a drawer."
  (interactive)
  (let* ((element (org-element-at-point))
         ;; Traverse up the AST to find an enclosing drawer
         (drawer (org-element-lineage element '(drawer property-drawer) t)))
    (when drawer
        (save-excursion
          ;; Move to the start of the drawer and un-flag (reveal) it
          (goto-char (org-element-property :begin drawer))
          (org-flag-drawer nil drawer)))))

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
