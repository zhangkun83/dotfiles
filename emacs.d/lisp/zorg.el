(require 'zk)
(require 'zk-org)
(require 'org)
(require 'org-element)
(require 'org-tempo)
(require 'dash)
(require 'queue)
(require 'cl-seq)
(require 'cl-lib)
(require 'pulse)

(when (display-graphic-p)
  (setq leuven-scale-outline-headlines nil
        leuven-scale-org-agenda-structure nil)
  (load-theme 'leuven t)
  (zk-org-init-fonts))

(defvar zk-zorg-rsync-backup-dir
  nil "The remote path used by rsync for backing up org files")

;; Use relative directory for local paths, because I may be running
;; this script under MINGW64 (provided by Git bash) whose home is like
;; "/c/Users/zhangkun" while the rsync on the system is a Cygwin
;; version whose home is like "/cygdrive/c/home/zhangkun". Passing the
;; absolute path from this script to rsync won't work.
(defvar zk-zorg-profile-name nil "Used as the local directory,
  relative to the home directory, to store org files.  Also used
  as the frame title and emacs server name.")

(defvar zk-zorg-startup-view-func nil "Called to present the
initial view once initialization has succeeded")

(defconst zk-zorg-rsync-buffer-name "*zorg rsync*")
(defconst zk-zorg-rsync-diff-buffer-name "*zorg diff*")

(defun zk-zorg-directory ()
  "Returns the absolute directory for local org files"
  (concat zk-user-home-dir "/" zk-zorg-profile-name))

;; Possible values: init, outdated, downloading, uploading, clean, modified, dirty
(setq zk-zorg-status 'init)

;; Most values are copied from the default mode-line-format. I added
;; zk-zorg-status.
(setq-default
 mode-line-format
 '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
   mode-line-frame-identification mode-line-buffer-identification " " mode-line-position
   " " (:eval (format "[%s]" zk-zorg-status))
   " " mode-line-modes
   mode-line-misc-info mode-line-end-spaces))

(defun zk-zorg-generate-upload-list-file ()
  "Generates a file that has the list of files eligible for
uploading. It only include org and org_archive files, and exclude
Emacs temporary files (starting with #) and hidden
files (starting with .). Returns the list file name."
  (let* ((list-file-name ".upload-list")
         (list-file (concat (zk-zorg-directory) "/" list-file-name))
         (file-list (directory-files (zk-zorg-directory))))
    (with-current-buffer (get-buffer-create "*zorg upload list*")
      (erase-buffer)
      (dolist (file file-list)
        (when (string-match-p "^[^.#].+\\.\\(org\\)\\|\\(org_archive\\)$" file)
          (insert file "\n")))
      ;; Call with VISIT=1 to not display the "Write file" message
      (write-region nil nil list-file nil 1)
      (kill-buffer))
    list-file-name))

(defun zk-zorg-goto-latest-note-file ()
  "Go to the latest note org file under the same directory."
  (interactive)
  (zk-zorg-locate-note-file 'last))

(defun zk-zorg-goto-next-note-file ()
  "Go to the next note org file under the same directory."
  (interactive)
  (zk-zorg-locate-note-file 'next))

(defun zk-zorg-goto-prev-note-file ()
  "Go to the previous note org file under the same directory."
  (interactive)
  (zk-zorg-locate-note-file 'prev))

(defun zk-zorg-list-note-files ()
  "Return the list all the note org files under the zorg directory."
  (directory-files (zk-zorg-directory) nil "notes.*\\.org$"))
  
(defun zk-zorg-locate-note-file (mode)
  "Go to the next note org file under the same directory.

MODE: `next': go to the next file
      `prev': go to the previous file
      `last': go to the last file"
  ;; directory-files sorts the files alphabeticaly
  (let* ((file-list (zk-zorg-list-note-files))
         (current-file-pos (when (buffer-file-name)
                             (cl-position
                              (file-name-nondirectory (buffer-file-name))
                              file-list
                              :test #'string=)))
         (num-files (length file-list)))
    (unless file-list (user-error "No notes file found"))
    (let ((file
           (cond ((eq mode 'last)
                  (car (last file-list)))
                 ((eq mode 'next)
                  (unless current-file-pos
                    (user-error "Current buffer is not a notes file"))
                  (if (= current-file-pos (- num-files 1))
                      (user-error "Already the last file.")
                    (nth (+ current-file-pos 1) file-list)))
                 ((eq mode 'prev)
                  (unless current-file-pos
                    (user-error "Current buffer is not a notes file"))
                  (if (= current-file-pos 0)
                      (user-error "Already the first file.")
                    (nth (- current-file-pos 1) file-list)))
                 (t (user-error "Invalid mode %s" mode)))))
      (let ((path (concat (zk-zorg-directory) "/" file)))
        (switch-to-buffer
         (or (find-buffer-visiting path)
             (find-file-noselect path)))))))

(defun zk-org-generate-custom-id-from-text (text)
  "Generate a plain ID that only contains alphanumerics and
underscores from a natural text. Throw an error if the generated
CUSTOM_ID already exists in the file."
  (let ((new-id
         (concat
          (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
          "_"
          (downcase
           (replace-regexp-in-string
            "\\`_*" ""  ; remove leading underscores
            (replace-regexp-in-string
             "_*\\'" ""  ; remove trailing underscores
             (replace-regexp-in-string  ; replace non alphanumerics to underscores
              "[^a-zA-Z0-9_]+" "_" text)))))))
    (if (zk-org-custom-id-exists-p new-id)
        (user-error "CUSTOM_ID \"%s\" already exists in the file. Try changing the headline to make it unique."
                    new-id))
    new-id))

(defun zk-org-custom-id-exists-p (custom-id)
  "Check if the given CUSTOM_ID already exists in the current org file."
  (let ((found-p nil))
    (org-map-entries (lambda ()
                       (if (string= custom-id
                               (org-element-property :CUSTOM_ID (org-element-at-point)))
                           (setq found-p t))))
    found-p))
  
(defun zk-org-copy-external-link (&optional arg)
  "Copy the external link based on the CUSTOM_ID to the kill ring.
When called with the prefix argument, the link will include
zk-zorg-profile-name so that it can be used for scratch.el"
  (interactive "P")
  (let* ((link-pair (zk-zorg-set-customid-and-get-headline-link-at-point arg))
         (link (nth 0 link-pair))
         (link-with-text (format "[[%s][^]]" link)))
    (kill-new link-with-text)
    (message "Copied \"%s\"" link-with-text)))

(defun zk-org-copy-external-reference (&optional arg)
  "Copy a reference, with the headline string followed by a link
based on the CUSTOM_ID, to the kill ring.  When called with the
prefix argument, the link will include zk-zorg-profile-name so
that it can be used for scratch.el"
  (interactive "P")
  (let ((reference (zk-org-get-external-reference arg)))
    (kill-new reference)
    (message "Copied \"%s\"" reference)))

(defun zk-org-get-external-reference (&optional arg)
  "Returns a reference, with the headline string followed by a link
based on the CUSTOM_ID.  When called with the prefix argument,
the link will include zk-zorg-profile-name so that it can be used
for scratch.el"
  (let* ((link-pair (zk-zorg-set-customid-and-get-headline-link-at-point arg))
         (link (nth 0 link-pair))
         (headline-text (nth 1 link-pair))
         (reference (format "%s %s[[%s][^]]"
                            headline-text
                            (if arg (concat "#" zk-zorg-profile-name) "")
                            link)))
    reference))

(defun zk-zorg-copy-region-with-link-to-heading (&optional arg)
  "Copy the content of the current active region, with a
link to the current headline.  By default the content of the headline
won't be included, but the first timestamp will be included.  When
called with the prefix argument, the heading text is also included.

This is useful for copying contents from a note entry to a task."
  (interactive "P")
  (let* ((mark-was-active mark-active)
         (link-pair (progn
                      (deactivate-mark)
                      (zk-zorg-set-customid-and-get-headline-link-at-point nil)))
         (link (nth 0 link-pair))
         (headline-text (nth 1 link-pair))
         ;; The "<>" and "[]" have been converted to "()" by
         ;; zk-zorg-set-customid-and-get-headline-link-at-point using
         ;; zk-org-neutralize-timestamp
         (timestamp-pos
          (string-match
           "(\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][^)]*\\))"
           headline-text))
         (backlink
          (format "([[%s][%s]])"
                  link
                  (if timestamp-pos
                      (match-string 1 headline-text)
                    (user-error "The heading doesn't contain a timestamp"))))
         (headline-without-timestamp
          (concat
           " "
           (string-trim
            (if timestamp-pos
                (substring headline-text 0 timestamp-pos)
              headline-text))
           )))
    (kill-new (concat
               backlink
               (if arg headline-without-timestamp "")
               "\n"
               (if mark-was-active
                   (buffer-substring (region-beginning) (region-end))
                 "")))
    (message "Copied %sbacklink to this headline."
             (if mark-active "region with " ""))))

(defun zk-org-get-scratch-reference-metadata ()
  "Returns a list of two elements representing an external reference
to be used in the scratch.  The first element is the text content
and the second element is the reference ID."
  (let* ((content (zk-org-get-external-reference t))
         (link-pair (zk-zorg-set-customid-and-get-headline-link-at-point t))
         (id (nth 0 link-pair)))
    (list content id)))

(defun zk-org-locate-in-scratch-task-queue ()
  "Locate the current heading in the task queues of the scratch
server."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((id (zk-org-get-customid-at-point)))
      (unless id
        (error "Entry doesn't have CUSTOM_ID"))
      (message "%s"
               (server-eval-at
                "scratch"
                (list 'zk-scratch-remote-locate-in-task-queue
                      (zk-org-generate-link id t)))))))

(defun zk-org-fill-scratch-task-queue ()
  "Insert all undone TODO entries with priority A and scheduled for
today to the task queues of the scratch server, if they don't
exist in the queue yet."
  (interactive)
  (let ((reference-metadata-list
         (--filter
          it ; this is the filter condition, effectively excluding nil values
          (org-map-entries
           (lambda ()
             (let* ((headline (org-element-at-point))
                    (todo-type (org-element-property :todo-type headline))
                    (priority (org-element-property :priority headline))
                    (scheduled-for-today-p (zk-org-scheduled-for-today-p headline)))
               (when (eq todo-type 'todo)
                 (let ((category
                        (cond (scheduled-for-today-p
                               (concat "Scheduled for " (format-time-string "%Y-%m-%d" (current-time))))
                              ((eq priority ?A) "Priorities")
                              ((eq priority ?B) "Back burner"))))
                   (when category
                     (zk-zorg-set-customid-at-point)
                     ;; Convert ("content" "id") to ('list "category"
                     ;; "content" "id"), so that it can be evaluated on
                     ;; the scratch server.
                     (cons 'list (cons category (zk-org-get-scratch-reference-metadata))))))))
           t
           'agenda-with-archives))))
    (message "%s"
             (server-eval-at
              "scratch"
              (list 'zk-scratch-remote-insert-all-to-task-queue
                    ;; Convert ((list "c1 "id1") (list "c2" "id2)) to
                    ;; (list (list "c1 "id1") (list "c2" "id2)) so that
                    ;; it can be evaluated on the scratch server
                    (cons 'list reference-metadata-list))))))

(defun zk-org-clone-narrowed-buffer ()
  "Clone the current org buffer and narrow to the current
subtree"
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Not in org-mode"))
  (let ((headline-text (org-get-heading t t t t))
        (pos (point)))
    (clone-indirect-buffer
     (concat "* " headline-text " * "
             (buffer-name (zk-get-base-buffer (current-buffer))))
     t)
    (goto-char pos)
    (org-narrow-to-subtree)))

(defun zk-zorg-set-customid-at-point ()
  "If CUSTOM_ID of the current org headline doesn't exist,
generate one based on the text of the headline and set it.  Returns the
CUSTOM_ID.  Ask for confirmation before setting the CUSTOM_ID."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Not in org-mode"))
  (save-excursion
    (org-back-to-heading)
    (let* ((heading-point (point))
           (headline (org-element-at-point))
           (headline-text (substring-no-properties (org-get-heading t t t t)))
           (custom-id (or
                       (org-element-property :CUSTOM_ID headline)
                       (let ((new-id
                              (zk-org-generate-custom-id-from-text headline-text)))
                         (save-window-excursion
                           (set-window-point
                            (display-buffer (current-buffer)) heading-point)
                           (barf-if-buffer-read-only)
                           (unless (y-or-n-p (format
                                              "Set CUSTOM_ID to '%s'?"
                                              headline-text))
                             (user-error "CUSTOM_ID rejected by user."))
                           (org-set-property "CUSTOM_ID" new-id))
                         new-id))))
      (when (called-interactively-p 'any)
        (message "CUSTOM_ID: %s" custom-id))
      custom-id)))

(defun zk-org-generate-link (custom-id with-profile-name)
  (concat "file:"
          (if with-profile-name (concat "@" zk-zorg-profile-name ":"))
          (file-name-nondirectory (buffer-file-name (zk-get-base-buffer (current-buffer))))
          "::#"
          custom-id))

(defun zk-zorg-eval-at-heading-pos (form)
  "When in org-mode, eval the given `form' as-is.  When in
org-agenda-mode or any other buffer that list headings, eval the given
`form' at the actual heading in the referenced org-mode buffer."
  (cond
   ((eq major-mode 'org-mode)
    (if zk-zorg-reference-tree-refresh-form
        ;; In a reference tree buffer
        (let ((pos-alist (zk-zorg-reference-tree--get-heading-pos-at-point)))
          (with-current-buffer (alist-get ':buffer pos-alist)
            (save-excursion
              (goto-char (alist-get ':point pos-alist))
              (eval form))))
      ;; In plain org-mode buffer
      (eval form)))
   ((eq major-mode 'org-agenda-mode)
    (let ((agenda-marker (get-text-property (point) 'org-marker)))
      (unless agenda-marker (user-error "Not a heading."))
      (with-current-buffer (marker-buffer agenda-marker)
        (save-excursion
          (goto-char agenda-marker)
          (eval form)))))
   (t (user-error "Not in org-mode or org-agenda-mode."))))

(defun zk-zorg-set-customid-and-get-headline-link-at-point (with-profile-name)
  (zk-zorg-eval-at-heading-pos
   `(zk-zorg-set-customid-and-get-headline-link-at-point-in-org-mode ,with-profile-name)))

(defun zk-zorg-set-customid-and-get-headline-link-at-point-in-org-mode (with-profile-name)
  "Returns a list of (link headline), where link is the external
link based on the CUSTOM_ID, and headline is the headline text.
Creates and sets the CUSTOM_ID if doesn't exist.  When
with-profile-name is non-nil, the link will include
zk-zorg-profile-name so that it can be used for scratch.el"
  (cl-assert (eq major-mode 'org-mode) t)
  (let ((return-value nil)
        (buffer (current-buffer)))
    (save-excursion
      (org-back-to-heading)
      (let* ((headline (org-element-at-point))
             (headline-text (substring-no-properties (org-get-heading t t t t)))
             (custom-id (zk-zorg-set-customid-at-point))
             (link (zk-org-generate-link custom-id with-profile-name)))
        (setq return-value (list link (zk-org-neutralize-timestamp headline-text)))))
    return-value))

(defun zk-org-get-customid-at-point ()
  "Returns the CUSTOM_ID of the current org entry.  nil if CUSTOM_ID
is not there."
  (save-excursion
    (org-back-to-heading)
    (org-element-property :CUSTOM_ID (org-element-at-point))))

(defun zk-org-neutralize-timestamp (text)
  "Convert org timestemps like \"[2023-07-27 Thu 14:58]\" or
\"<2023-07-27 Thu 14:58>\" to a format that is not parsed by
org-mode, by changing the brackets and angel brackets to
parentheses."
  (let ((result (copy-sequence text))
        (date-pattern "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"))
    (while (string-match (concat "\\[" date-pattern "[^]]*\\]") result)
      (aset result (match-beginning 0) ?\()
      (aset result (- (match-end 0) 1) ?\)))
    (while (string-match (concat "<" date-pattern "[^>]*>") result)
      (aset result (match-beginning 0) ?\()
      (aset result (- (match-end 0) 1) ?\)))
    result))

(defun zk-org-get-link-at-point()
  (let ((link-prop (get-text-property (point) 'htmlize-link)))
    (when link-prop
      (nth 1 link-prop))))

(defun zk-org-insert-tag-completion ()
  "Insert a tag to the current buffer with completion"
  (interactive)
  ;; Search forward for the first portion of the tag that the user may
  ;; have typed.
  (let ((tag-begin (point))
        (point (point)))
    (save-excursion
      (while (zk-org-valid-tag-char-p (char-before))
        (backward-char))
      (setq tag-begin (point)))
    (let ((result (completing-read
                   "-->Complete tag: "
                   (org-global-tags-completion-table)
                   nil
                   nil
                   (buffer-substring tag-begin point))))
      (when result
        (delete-region tag-begin point)
        (insert result)))))

(defun zk-org-tags-view (arg)
  "org-tags-view will always ask for the tags before switching to
an existing view buffer if available, but it doesn't use the
entered tags anyway if org-agenda-sticky is turned
on. zk-org-tags-view will always create a new buffer for the query."
  (interactive "P")
  (let ((tags (read-string "View for tags: " nil 'org-tags-history nil t)))
    (unless (> (length tags) 0)
      (user-error "No tag entered."))
    (org-tags-view arg tags)))

(defun zk-org-switch-to-tags-view-buffer ()
  "Let the user to select one of the preexisting tag search buffers in
MRU (most recently used) order,which is convenient for going back to the
previous search"
  (interactive)
  (zk-switch-to-buffer-from-filtered-list "^\\*Org Agenda(m:[^)]+)\\*"))

(defun zk-org-search-view (arg)
  "Like org-search-view but always create a new buffer for the
query."
  (interactive "P")
  (let ((search (read-string "View for search term: " nil 'org-agenda-search-history nil t)))
    (unless (> (length search) 0)
      (user-error "No tag string entered."))
    (org-search-view arg search)))

(defun zk-org-switch-to-search-view-buffer ()
  "Let the user to select one of the preexisting search view buffers in
MRU (most recently used) order,which is convenient for going back to the
previous search"
  (interactive)
  (zk-switch-to-buffer-from-filtered-list "^\\*Org Agenda(s:[^)]+)\\*"))

(defun zk-org-set-tags-command ()
  "Set tags to the current entry. It's better than
org-set-tags-command in that it uses the agenda files instead of
the current file for completion.  It keeps asking for new tag until
an empty line is entered."
  (interactive)
  (let ((new-tag nil))
    (while (not (equal new-tag ""))
      (setq new-tag (completing-read
                     (concat "Tags: " (org-make-tag-string (org-get-tags nil t)))
                     (org-global-tags-completion-table)
                     nil
                     nil
                     nil
                     t))
      (when (not (equal new-tag ""))
        (org-toggle-tag new-tag 'on)))))

(defun zk-zorg-populate-agenda-command ()
  "Find headings that have a `tbdsc' tag (which means to-be-discussed) and
also match another tag given by the user, popuplate them as agenda
items.  One agenda item includes a backlink to the topic, which is the
parent heading of the tbdsc heading, and the content of the tbdsc
heading that captures the agenda."
  (interactive)
  (let* ((all-tags (mapcar #'car (org-global-tags-completion-table)))
         (tag (completing-read
               "Populate agenda for tag: "
               all-tags
               nil
               t
               nil
               t))
         (items nil)
         (org-use-tag-inheritance nil))
    (org-map-entries
     (lambda ()
       (let ((topic-link
              (save-excursion
                (org-up-element)
                (zk-org-get-external-reference)))
             (agenda-title
              (org-element-property :title (org-element-at-point)))
             (agenda-content
              (save-mark-and-excursion
                (zk-org-mark-heading-content)
                (string-trim
                 (buffer-substring-no-properties (region-beginning) (region-end))))))
         (let ((new-item (concat "RE: " topic-link "\n"
                                 "- *" agenda-title "*\n"
                                 (if (equal "" agenda-content) ""
                                   (concat agenda-content "\n"))
                                 "- ---- -\n\n")))
           ;; When CUSTOM_ID is added during the process, the position
           ;; of the entry is shifted down, which could cause the
           ;; entry to be visited again, causing duplicates unless we
           ;; explicitly check for them.
           (unless (member new-item items)
             (push new-item items)))))
     (concat "tbdsc" "+" tag)
     'agenda)
    (dolist (item items)
      (insert item)
      (newline))
    (message "Populated %d agenda items" (length items))))


(defun zk-org-rename-tag-command ()
  "Rename a tag throughout the agenda files."
  (interactive)
  ;; Save all files before the renaming, to give the user a chance to
  ;; discard the rename if they change their mind.
  (save-some-buffers)
  (let* ((all-tags (mapcar #'car (org-global-tags-completion-table)))
         (from-tag (completing-read
                    "Rename tag: "
                    all-tags
                    nil
                    t
                    nil
                    t))
         (to-tag (zk-trim-string
                  (read-string (format "Rename tag \"%s\" to (empty string to delete): " from-tag) from-tag t)))
         (counter 0))
    (when (cond ((string= from-tag to-tag)
                 (progn (message "The new tag is the same as the old tag") nil))
                ((string= "" to-tag)
                 (yes-or-no-p
                  (format "Do you want to remove tag \"%s\" from all entries?" from-tag)))
                ((-contains? all-tags to-tag)
                 (yes-or-no-p
                  (format "Do you want to merge \"%s\" into \"%s\"?" from-tag to-tag)))
                (t t))
      (org-map-entries
       (lambda ()
         (when (-contains? (org-get-tags nil t) from-tag)
           (org-toggle-tag from-tag 'off)
           (setq counter (+ 1 counter))
           (unless (string= "" to-tag)
             (org-toggle-tag to-tag 'on))))
       t
       'agenda-with-archives)
      (if (string= "" to-tag)
          (message "Removed \"%s\" in %d entries" from-tag counter)
        (message "Changed \"%s\" to \"%s\" in %d entries" from-tag to-tag counter)))))

(defun zk-zorg-show-status ()
  (interactive)
  (message "%s status: %s" zk-zorg-profile-name zk-zorg-status))

(defun zk-zorg-open-tbs-agenda ()
  "Open the org agenda for tag `tbs' (to-be-sorted) for raw meeting notes
that need to be sorted."
  (interactive)
  (org-tags-view nil "tbs"))

(defun zk-org-setup-bindings ()
  "Register my own shortcuts for org mode"
  (local-set-key (kbd "C-c a") 'org-agenda-list)
  (local-set-key (kbd "C-c t") 'org-todo-list)
  (local-set-key (kbd "C-c M-t") 'zk-zorg-open-tbs-agenda)
  (local-set-key (kbd "C-c m") 'zk-org-tags-view)
  (local-set-key (kbd "C-c M-m") 'zk-org-switch-to-tags-view-buffer)
  (local-set-key (kbd "C-c s") 'zk-org-search-view)
  (local-set-key (kbd "C-c M-s") 'zk-org-switch-to-search-view-buffer)
  (local-set-key (kbd "C-c q") 'zk-org-set-tags-command)
  (local-set-key (kbd "C-c l i") 'zk-zorg-set-customid-at-point)
  (local-set-key (kbd "C-c l a") 'zk-zorg-populate-agenda-command)
  (local-set-key (kbd "C-c l l") 'zk-org-copy-external-link)
  (local-set-key (kbd "C-c l r") 'zk-org-copy-external-reference)
  (local-set-key (kbd "C-c l w") 'zk-zorg-copy-region-with-link-to-heading)
  (local-set-key (kbd "C-c l f") 'zk-zorg-reference-tree-command)
  (local-set-key (kbd "C-c l C-f") 'zk-zorg-reference-trees-for-tags-command)
  (local-set-key (kbd "C-c l M-f") 'zk-zorg-reference-trees-switch-to-buffer)
  (local-set-key (kbd "C-c l s") 'zk-org-locate-in-scratch-task-queue)
  (local-set-key (kbd "C-c l C-s") 'zk-org-fill-scratch-task-queue)
  (local-set-key (kbd "C-c r s") 'zk-zorg-show-status)
  (local-set-key (kbd "C-c r u") 'zk-zorg-rsync-upload)
  (local-set-key (kbd "C-c r d") 'zk-zorg-rsync-download)
  (local-set-key (kbd "C-c r C-d") 'zk-zorg-rsync-diff)
  (local-set-key (kbd "C-c r o") 'zk-zorg-set-outdated)
  (local-set-key (kbd "C-c e h") 'zk-org-export-html-to-clipboard)
  (local-set-key (kbd "C-c o") 'zk-org-open-next-link)
  (local-set-key (kbd "C-c n n") 'zk-zorg-goto-next-note-file)
  (local-set-key (kbd "C-c n p") 'zk-zorg-goto-prev-note-file)
  (local-set-key (kbd "C-c z i") 'zk-zorg-ai-use-current-entry-as-input)
  (local-set-key (kbd "C-c z SPC") 'zk-zorg-ai-goto-original-input-pos)
  (local-set-key (kbd "C-c z o") 'zk-zorg-ai-view-output)
  (local-set-key (kbd "C-c z p") 'zk-zorg-ai-generate-gemini-cli-prompts)
  (local-set-key (kbd "C-c c") 'zk-org-clone-narrowed-buffer))

(defun zk-org-set-file-encoding ()
  ;; Force unix newline format, even on Windows
  (setq buffer-file-coding-system 'utf-8-unix))

(defun zk-zorg-startup-init ()
  "Initializes zorg session."
  (zk-start-server-or-create-frame zk-zorg-profile-name)
  (unless zk-zorg-rsync-backup-dir
    (user-error "zk-zorg-rsync-backup-dir not set"))
  (unless zk-zorg-profile-name
    (user-error "zk-zorg-profile-name not set"))
  (setq default-directory (zk-zorg-directory))
  (setq org-agenda-files (list (zk-zorg-directory))
        org-agenda-file-regexp "\\`[^.+].*\\.org\\'"
        org-directory (zk-zorg-directory)
        zk-frame-title-base-name zk-zorg-profile-name)
  (setq org-agenda-span 'fortnight)
  (setq org-agenda-show-all-dates t)
  (setq org-deadline-warning-days 180)
  (setq confirm-kill-emacs 'zk-zorg-shutdown-confirm)
  (zk-zorg-set-outdated)
  (zk-zorg-rsync-download)
  (zk-zorg-startup-open))

(defun zk-zorg-rsync-download ()
  "Download zorg files from remote.  Update the zorg status
 according to the result."
  (interactive)
  (when (zk-has-unsaved-files-p)
    (user-error "There are unsaved files."))
  (unless (eq zk-zorg-status 'outdated)
    (user-error "Cannot download when status is %s" zk-zorg-status))
  (setq zk-zorg-status 'downloading)
  (let ((default-directory (zk-zorg-directory)))
    (switch-to-buffer zk-zorg-rsync-buffer-name)
    (with-current-buffer zk-zorg-rsync-buffer-name
      (erase-buffer)
      (zk-log-to-current-buffer "Downloading %s files ..." zk-zorg-profile-name)
      (if (eq 0 (call-process "rsync" nil zk-zorg-rsync-buffer-name t
                              "--whole-file"
                              "-rtuv" (concat zk-zorg-rsync-backup-dir "/") "."))
          (progn
            (zk-log-to-current-buffer "Download successful.")
            (if (zk-zorg-rsync-check-remote-consistency)
                (progn
                  (remove-hook 'org-mode-hook 'zk-zorg-make-buffer-read-only)
                  (mapc (function
                         (lambda (buf) (with-current-buffer buf
                                         (when (zk-zorg-org-file-p)
                                           (read-only-mode -1)
                                           (revert-buffer t t)))))
                        (buffer-list))
                  (setq zk-zorg-status 'clean))
              (setq zk-zorg-status 'dirty)))
        (setq zk-zorg-status 'outdated)
        (zk-log-to-current-buffer "Download failed.")
        (read-string "Press Enter to continue ...")
        (kill-buffer)))))

(defun zk-zorg-rsync-upload ()
  (interactive)
  (when (zk-has-unsaved-files-p)
    (user-error "There are unsaved files."))
  (unless (or
           (eq zk-zorg-status 'clean)
           (eq zk-zorg-status 'modified))
    (user-error "Cannot upload when status is %s" zk-zorg-status))
  (let ((default-directory (zk-zorg-directory)))
    (setq zk-zorg-status 'uploading)
    (switch-to-buffer zk-zorg-rsync-buffer-name)
    (with-current-buffer zk-zorg-rsync-buffer-name
      (erase-buffer)
      (zk-log-to-current-buffer "Uploading local changes ...")
      (if (eq 0 (call-process "rsync" nil zk-zorg-rsync-buffer-name t
                              "--whole-file"
                              "-rtuv"
                              (concat "--files-from=" (zk-zorg-generate-upload-list-file))
                              "./" zk-zorg-rsync-backup-dir))
          (progn
            (setq zk-zorg-status 'clean)
            (zk-log-to-current-buffer "Upload successful.")
            (read-string "Press Enter to continue ..."))
        (setq zk-zorg-status 'modified)
        (zk-log-to-current-buffer "Upload failed.")
        (read-string "Press Enter to continue ..."))
      (kill-buffer))))

(defun zk-zorg-rsync-diff ()
  "Display the diff of the local files against the remote files."
  (interactive)
  (when (zk-has-unsaved-files-p)
    (user-error "There are unsaved files."))
  (let ((temp-directory (concat (zk-zorg-directory) ".tmp-diff-remote"))
        (output-buffer zk-zorg-rsync-diff-buffer-name))
    (mkdir temp-directory t)
    (let ((default-directory temp-directory))
      (when (get-buffer output-buffer)
        (kill-buffer output-buffer))
      (switch-to-buffer output-buffer)
      (insert "#### Downloading remote files ...\n")
      (unless (eq 0 (call-process "rsync" nil output-buffer t
                                  "--whole-file"
                                  "-crti" "--delete" (concat zk-zorg-rsync-backup-dir "/") "."))
        (error "Failed to download remote files"))
      (insert "\n#### Generating diff ...\n"))
    (call-process "diff" nil output-buffer t
                  "-ur" temp-directory (zk-zorg-directory))
    (insert "#### End of diff.\n")
    (diff-mode)
    (read-only-mode 1)))

(defun zk-zorg-rsync-check-remote-consistency ()
  "Returns t if the local files are consistent with the remote
files, nil if check failed."
  (let ((default-directory (zk-zorg-directory))
        (do-it-p t)
        (consistent-p nil))
    (while do-it-p
      (switch-to-buffer zk-zorg-rsync-buffer-name)
      (with-current-buffer zk-zorg-rsync-buffer-name
        (zk-log-to-current-buffer "Checking consistency ...")
        (let ((original-lines (count-lines (point-min) (point-max))))
          (if (eq 0 (call-process "rsync" nil zk-zorg-rsync-buffer-name t
                                  "--whole-file"
                                  "-ncrti"
                                  (concat "--files-from=" (zk-zorg-generate-upload-list-file))
                                  "./" zk-zorg-rsync-backup-dir))
              (progn
                (setq do-it-p nil)
                (if (> (count-lines (point-min) (point-max)) original-lines)
                    (zk-log-to-current-buffer "WARNING: local files differ from remote.  Please check.")
                  (zk-log-to-current-buffer "Local files are consistent with remote.")
                  (read-string "Press Enter to continue ...")
                  (kill-buffer)
                  (setq consistent-p t)))
            (zk-log-to-current-buffer "Failed to check remote consistency.")
            (unless (y-or-n-p "Retry? ")
              (setq do-it-p nil))))))
    consistent-p))

(defun zk-zorg-org-file-p ()
  "Return non-nil if the current buffer, or its base buffer, is a
org file."
  (let ((base-buffer (zk-get-base-buffer (current-buffer))))
    (with-current-buffer base-buffer
        (and (eq major-mode 'org-mode)
             (buffer-file-name)))))

(defun zk-zorg-make-buffer-read-only ()
  (when (zk-zorg-org-file-p)
    (read-only-mode 1)))

(defun zk-zorg-set-outdated ()
  "Set the zorg status to `outdated`.  Can be called only if the
 status is `init`, `downloading` or `clean`.  This is useful when we are
about to modify the files from a different location, and don't want
to close the current sessions."
  (interactive)
  (when (zk-has-unsaved-files-p)
    (user-error "There are unsaved files."))
  (unless (or (eq zk-zorg-status 'init)
              (eq zk-zorg-status 'downloading)
              (eq zk-zorg-status 'clean))
    (user-error "Cannot transit from %s to outdated"
                zk-zorg-status))
  (setq zk-zorg-status 'outdated)
  (add-hook 'org-mode-hook 'zk-zorg-make-buffer-read-only)
  (mapc (function
         (lambda (buf) (with-current-buffer buf
                         (zk-zorg-make-buffer-read-only))))
        (buffer-list))
  (message "%s is now outdated." zk-zorg-profile-name))

(defun zk-zorg-startup-open ()
  (when (or (eq zk-zorg-status 'clean)
            (eq zk-zorg-status 'outdated))
    (when zk-zorg-startup-view-func
      (message "Opening the initial view ...")
      (funcall zk-zorg-startup-view-func))
    (message "Ready (%s). Have a very safe and productive day!"
             zk-zorg-status)))

(defun zk-zorg-link-open-from-string (link)
  "Like org-link-open-from-string, but will always do it from the
 zorg directory."
  (let ((default-directory (zk-zorg-directory)))
    (message "Opening %s" link)
    (org-link-open-from-string link)))

(defun zk-zorg-shutdown-confirm (prompt)
  (if (eq zk-zorg-status 'modified)
      (string-equal
       (read-string "Some modifications have not been uploaded. Type \"I want to quit!\" if you really want to quit: ")
       "I want to quit!")
    (yes-or-no-p prompt)))

(defun zk-zorg-current-buffer-is-zorg-file-p ()
  (let ((file-name (file-name-nondirectory buffer-file-name)))
    (and
     ;; Only monitor changes in zorg files
     (member file-name (directory-files (zk-zorg-directory)))
     ;; Exclude ".upload-list" and other dot files
     (not (string-prefix-p "." file-name)))))


;; Prevent modified file from being saved in outdated status.  This is
;; another layer of protection in addition to the read-only mode,
;; because I found sometimes some strange change got in even though
;; the buffer is read-only.
(add-to-list
 'write-file-functions
 (lambda()
   (when (and
          (buffer-modified-p)
          (eq zk-zorg-status 'outdated)
          (zk-zorg-current-buffer-is-zorg-file-p))
     (error "File modified in outdated status, which shouldn't have happened"))))

(add-hook 'after-save-hook
          (lambda ()
            (if (and (eq zk-zorg-status 'clean)
                     (zk-zorg-current-buffer-is-zorg-file-p))
                (setq zk-zorg-status 'modified))))

(defun zk-org-get-heading-string ()
  "Returns the current org heading string.  nil if not under an
 org heading."
  (ignore-errors
    (nth 4 (org-heading-components))))


(defun zk-zorg-search-activities-since (start-date &optional tag)
  "Search for headings that have timestamps after the given start-date, optionally
filtered by a tag."
  (let ((start_date (format "<%s>" start-date))
        (tags_suffix (if tag (concat "&+" tag) "")))
    (org-tags-view nil
                   (format "TIMESTAMP_IA>=\"%s\"%s|TIMESTAMP>=\"%s\"%s"
                           start_date tags_suffix start_date tags_suffix))))

;; Back reference tree implementation

(defun zk-zorg-reference-tree--get-current-heading-link ()
  (or (zk-org-get-current-heading-link)
      (error "Failed to get heading link at pos %d of %s"
                  (point) (buffer-file-name))))

(defun zk-zorg-reference-tree--create-entry-alist-for-current-entry ()
  "Create an alist for the current heading, which contains keys (:link :todo-keyword :title :file :tags :custom-id :file-path :pos)."
  (save-excursion
    (org-back-to-heading)
    (let* ((element (or (org-element-at-point)
                        (error "No heading found")))
           (heading-link (zk-zorg-reference-tree--get-current-heading-link))
           (custom-id (zk-org-get-customid-at-point))
           (file-path (buffer-file-name))
           (file (file-name-nondirectory file-path))
           (todo-keyword (org-element-property :todo-keyword element))
           (tags (org-get-tags))  ; Use org-get-tags to include inherited tags
           (title (zk-org-neutralize-timestamp (org-element-property :title element))))
      (list (cons ':link heading-link)
            (cons ':todo-keyword todo-keyword)
            (cons ':title title)
            (cons ':file-path file-path)
            (cons ':file file)
            (cons ':pos (point))
            (cons ':tags tags)
            (cons ':custom-id custom-id)))))

(defconst zk-zorg-reference-tree-link-regex
  "\\[\\[file:[^][:#]*::#\\([^][:#]*\\)\\]\\[\\([^][:#]*\\)\\]"
  "The links are in the format
\"[[file:notes2024q1.org::#ramp_up_on_mitigation_engine_work][link]]\"
where \"ramp_up_on_mitigation_engine_work\" is the ID to be extracted.
The CUSTOM_ID is generated by `zk-org-generate-link'.")

(defun zk-zorg-reference-trees-for-tags--find-root-entries (tag-match)
  "Scan the whole repo and create a list that contains the alists of
entries that match the given tag-match string and don't contain any back
references (with \"RE:\").  Those are considered root entries.  The
resulting list is set to the buffer-local variable
`zk-zorg-reference-tree-root-entry-alists'."
  (let ((root-entry-list nil)
        ;; The back references are in the format "RE: Foo bar
        ;; [[link][text]]".  There may be at most 3 newlines between
        ;; the "RE: " and the link.
        (backref-link-regex
         (concat "RE: .*\\(?:\n.*\\)\\{0,3\\}"
                 zk-zorg-reference-tree-link-regex)))
    (org-map-entries
     (lambda ()
       (let ((entry-alist
              (zk-zorg-reference-tree--create-entry-alist-for-current-entry)))
         (save-mark-and-excursion
           (zk-org-mark-heading-content)
           (unless (re-search-forward backref-link-regex (region-end) t)
             (push entry-alist root-entry-list)))))
     tag-match
     'agenda-with-archives)
    (setq zk-zorg-reference-tree-root-entry-alists
          root-entry-list)))

(defun zk-zorg-reference-tree--create-index ()
  "Scan the whole repo and create the index for creating reference trees.
It creates a multimap where the key are entry IDs (CUSTOM_ID), and the
values are alists (:link :todo-keyword :title :file :tags) of the
heading entries that contain references to the key ID.  The result is
set to the buffer-local variables
`zk-zorg-reference-tree-destid-to-src-entry-mp'"
  (let ((id-to-link-multimap (make-hash-table :test 'equal))
        (link-regex zk-zorg-reference-tree-link-regex))
    (org-map-entries
     (lambda ()
       (let ((entry-alist
              (zk-zorg-reference-tree--create-entry-alist-for-current-entry)))
         (save-mark-and-excursion
           (zk-org-mark-heading-content)
           ;; Search for CUSTOM_ID references
           (while (re-search-forward link-regex (region-end) t)
             (let ((dest-id (match-string-no-properties 1))
                   ;; link-text is not used for now, but may be used
                   ;; in the future
                   (link-text (match-string-no-properties 2)))
               (zk-multimap-add id-to-link-multimap
                                dest-id
                                ;; Save the pos pointing to the link in
                                ;; the body instead of the heading
                                (save-excursion
                                  (back-to-indentation)
                                  (cl-acons ':pos (point) entry-alist))))))))
     t
     'agenda-with-archives)
    (setq zk-zorg-reference-tree-destid-to-src-entry-mp
          id-to-link-multimap)))

(defun zk-zorg-reference-tree--print-entry
    (ancestor-links
     entry-alist
     print-src-entries-p)
  (let* ((level (length ancestor-links))
         (todo-keyword (alist-get ':todo-keyword entry-alist))
         (link (alist-get ':link entry-alist))
         (tags (alist-get ':tags entry-alist))
         (custom-id (alist-get ':custom-id entry-alist))
         (src-entry-alists
          (zk-multimap-get zk-zorg-reference-tree-destid-to-src-entry-mp
                           custom-id))
         (src-entry-alist-count (length src-entry-alists))
         (line-start-pos (point))
         (expansion-state 'unexpandable))
    (when (and (> src-entry-alist-count 0)
               ;; If this entry already appears in the ancestor list,
               ;; we don't make it expandable.
               (not (member link ancestor-links)))
      (if print-src-entries-p
          (setq expansion-state 'expanded)
        (setq expansion-state 'expandable)))
    (insert (make-string (* 2 level) ?\ ))
    (insert (if (eq expansion-state 'expandable)
                "+ "
              "- "))
    (insert
     (if todo-keyword (concat "[" todo-keyword "] ") "")
     (alist-get ':title entry-alist)
     (if tags (concat " \t:" (mapconcat 'identity tags ":") ":") "")
     " (" (alist-get ':file entry-alist) ")")
    ;; Attach entry-alist and ancestor-links to the whole line, to be
    ;; used by zk-zorg-reference-tree--expand-at-point
    (add-text-properties
     line-start-pos
     (point)
     `(zk-zorg-reference-tree-entry-alist
       ,entry-alist
       zk-zorg-reference-tree-entry-ancestor-links
       ,ancestor-links
       zk-zorg-reference-tree-entry-expansion-state
       ,expansion-state))
    (newline)
    (when (and print-src-entries-p custom-id)
      (dolist (src-entry-alist src-entry-alists)
        (zk-zorg-reference-tree--print-entry
         (cons link ancestor-links)
         src-entry-alist
         (not print-src-entries-p))))))


(defvar-local zk-zorg-reference-tree-refresh-form nil
  "The form to be evaluated to refresh the ref tree buffer")

(defvar-local zk-zorg-reference-tree-destid-to-src-entry-mp nil
  "The index of the backref tree.  It's a multimap from destination
ID (CUSTOM_ID) to the source entry alist.")

(defvar-local zk-zorg-reference-tree-root-entry-alists nil
  "The entry-alists of root entries.  A root entry is an entry that doesn't back
refer (with \"RE:\") to any other entries.")

(defface zk-zorg-backref-neutralized-timestamp
  `((t :height 0.9 :family ,zk-font-family :background "#EEEEEE"))
  "The face for neutralized timestamp (e.g., `(2025-01-23)') in backref buffer")

(defface zk-zorg-backref-tags
  `((t :height 0.9 :family ,zk-font-family :foreground "#9A9FA4"))
  "The face for tags in backref buffer")

(defface zk-zorg-backref-bullet-prefix
  `((t :height 0.9 :family ,zk-font-family :weight bold))
  "The face for the bullet character and the TODO keywords in backref buffer")

(defun zk-zorg-reference-tree--refresh ()
  (interactive)
  (when (yes-or-no-p "Do you want to refresh the ref tree (expansions will be reset)?")
    (eval zk-zorg-reference-tree-refresh-form)))

(defun zk-zorg-reference-tree--config-buffer (refresh-form)
  "Configure a newly created rertree buffer."
  (goto-char 0)
  (set-buffer-modified-p nil)
  (read-only-mode t)

  ;; Format different components of the content for better
  ;; readability.  This has to be done via font-lock, instead of
  ;; propertizing the output directly, because the propertization
  ;; would be overridden by org-mode font-lock anyway.
  (font-lock-add-keywords
   nil
   '(("^ *[+-] \\(\\[[A-Z]+\\]\\)?"
      . 'zk-zorg-backref-bullet-prefix)
     (":[a-zA-Z0-9_@#:]*:"
      . 'org-tag)
     ("([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][^)]*)"
      . 'zk-zorg-backref-neutralized-timestamp)))
  ;; Copy the key map to prevent from unintentionally modifying the
  ;; shared org-mode-map
  (let ((my-local-map (copy-keymap (current-local-map))))
    (use-local-map my-local-map)
    (define-key my-local-map (kbd "n") 'next-line)
    (define-key my-local-map (kbd "p") 'previous-line)
    (define-key my-local-map (kbd "q") 'quit-window)
    (define-key my-local-map (kbd "RET") 'zk-zorg-reference-tree--open-link)
    (define-key my-local-map (kbd "SPC") 'zk-zorg-reference-tree--open-link-other-window)
    (define-key my-local-map (kbd "TAB") 'zk-zorg-reference-tree--expand-at-point)
    (setq zk-zorg-reference-tree-refresh-form refresh-form)
    (define-key my-local-map (kbd "g") 'zk-zorg-reference-tree--refresh)))


(defun zk-zorg-reference-tree--get-heading-pos-at-point ()
  "Returns the position of the current heading in the form of alist (:buffer :point)."
  (let* ((entry-alist
         (get-text-property (point) 'zk-zorg-reference-tree-entry-alist)))
    (cl-assert entry-alist t)
    ;; org-link-open-from-string doesn't work reliably, thus we save
    ;; the absolute positions.
    (let ((file-path (alist-get ':file-path entry-alist))
          (pos (alist-get ':pos entry-alist)))
      (cl-assert file-path t)
      (cl-assert pos t)
      (let ((buffer (find-file-noselect file-path)))
        (cl-assert buffer t)
        (list (cons ':buffer buffer)
              (cons ':point pos))))))

(defun zk-zorg-reference-tree--open-link (&optional other-window)
  (interactive)
  (let* ((pos-alist (zk-zorg-reference-tree--get-heading-pos-at-point))
         (buffer (alist-get ':buffer pos-alist))
         (pos (alist-get ':point pos-alist)))
    (if other-window
        (let ((window (display-buffer buffer 'display-buffer-below-selected)))
          (set-window-point window pos)
          (save-window-excursion
            (select-window window)
            (pulse-momentary-highlight-one-line)))
      (org-mark-ring-push)
      (switch-to-buffer buffer)
      (goto-char pos)
      (pulse-momentary-highlight-one-line))))

(defun zk-zorg-reference-tree--open-link-other-window ()
  (interactive)
  (zk-zorg-reference-tree--open-link t))

(defun zk-zorg-reference-tree (entry-alist)
  (let* ((output-buffer
          (zk-recreate-buffer
           (concat "*zorg reftree* " (alist-get ':title entry-alist)))))
    (with-current-buffer output-buffer
      (org-mode)
      (zk-zorg-reference-tree--create-index)
      (zk-zorg-reference-tree--print-entry
       nil entry-alist t)
      (zk-zorg-reference-tree--config-buffer
       (list 'zk-zorg-reference-tree
             `(quote ,entry-alist)))
    (switch-to-buffer output-buffer))))
  

(defun zk-zorg-reference-tree-command ()
  "Create a buffer to display the reference tree of the current heading
entry (the starting entry).  A reference tree is a tree of heading
entries where the children of a node are the entries that contains links
point back to that node.

This is useful for exploring all the related entries, directly or
indirectly linking to the starting entry."
  (interactive)
  (zk-zorg-eval-at-heading-pos
  `(zk-zorg-reference-tree
     (zk-zorg-reference-tree--create-entry-alist-for-current-entry))))


(defun zk-zorg-reference-trees-for-tags (tag-match)
  "Create a buffer to display the reference trees of all root entries that
match the given tag-match string, and have at least one back references.
A root entry is an entry that doesn't back refer (with \"RE:\") to any
other entries."
  (let* ((output-buffer
          (zk-recreate-buffer
           (concat "*zorg reftree* m:" tag-match))))
    (with-current-buffer output-buffer
      (org-mode)
      (zk-zorg-reference-tree--create-index)
      (zk-zorg-reference-trees-for-tags--find-root-entries tag-match)
      (let ((progress-reporter
             (make-progress-reporter
              "Scanning root entries"
              0 (length zk-zorg-reference-tree-root-entry-alists)))
            (counter 0))
        (dolist (entry-alist zk-zorg-reference-tree-root-entry-alists)
          (when
              ;; Use the root-entry only if it has back references
              (let ((custom-id (alist-get ':custom-id entry-alist)))
                (and custom-id
                     (zk-multimap-get
                      zk-zorg-reference-tree-destid-to-src-entry-mp
                      custom-id)))
            (zk-zorg-reference-tree--print-entry
             nil entry-alist nil))
          (cl-incf counter)
          (progress-reporter-update progress-reporter counter))
        (progress-reporter-done progress-reporter))
      (zk-zorg-reference-tree--config-buffer
       (list 'zk-zorg-reference-trees-for-tags
             `(quote ,tag-match))))
    (switch-to-buffer output-buffer)))

(defun zk-zorg-reference-trees-for-tags-command ()
  (interactive)
  (let ((tags (read-string "Reference trees for tags: " nil 'org-tags-history nil t)))
    (unless (> (length tags) 0)
      (user-error "No tag entered."))
    (zk-zorg-reference-trees-for-tags tags)))

(defun zk-zorg-reference-trees-switch-to-buffer ()
  "Let the user to select one of the preexisting back reference buffers in
MRU (most recently used) order,which is convenient for going back to the
previous search"
  (interactive)
  (zk-switch-to-buffer-from-filtered-list "^\\*zorg reftree\\* "))

(defun zk-zorg-reference-tree--expand-at-point ()
  "Expand the entry at point in back ref buffer."
  (interactive)
  (let ((original-pos (point)))
    (unwind-protect
        (progn
          (let* ((entry-alist
                  (get-text-property (point) 'zk-zorg-reference-tree-entry-alist))
                 (entry-ancestor-links
                  (get-text-property (point) 'zk-zorg-reference-tree-entry-ancestor-links))
                 (level (length entry-ancestor-links))
                 (inhibit-read-only t)
                 (entry-expansion-state
                  (get-text-property (point) 'zk-zorg-reference-tree-entry-expansion-state)))
            (cl-assert entry-alist t)
            (cond
             ((eq entry-expansion-state 'expandable)
              (progn
                ;; Delete the original line, since it will be
                ;; reinserted when expanding
                (beginning-of-line)
                (delete-region (point) (progn (forward-line 1) (point)))
                (zk-zorg-reference-tree--print-entry
                 entry-ancestor-links entry-alist t)))
             ((eq entry-expansion-state 'expanded)
              (progn
                ;; Delete this line and all the items below it
                (beginning-of-line)
                (delete-region (point) (progn (forward-line 1) (point)))
                (while (>
                        (length (get-text-property
                                 (point)
                                 'zk-zorg-reference-tree-entry-ancestor-links))
                        level)
                  (delete-region (point) (progn (forward-line 1) (point))))
                ;; Re-insert the line in its unexpanded state
                (zk-zorg-reference-tree--print-entry
                 entry-ancestor-links entry-alist nil)))
             (t (user-error "This line is not expandable.")))))
      ;; save-excursion won't help if the original line was deleted
      ;; and re-inserted
      (goto-char original-pos))))

;; Generative AI (LLM) related

(defconst zk-zorg-ai-input-file-name ".tmp-ai-input.org")
(defconst zk-zorg-ai-output-file-name ".tmp-ai-output.org")

(defun zk-zorg-ai-input-file-path ()
  (concat (zk-zorg-directory) "/" zk-zorg-ai-input-file-name))

(defun zk-zorg-ai-output-file-path ()
  (concat (zk-zorg-directory) "/" zk-zorg-ai-output-file-name))

(defvar zk-zorg-ai-original-input-pos nil
  "The position as (buffer . pos) of the org entry that was last used by
`zk-zorg-ai-use-current-entry-as-input'")

(defun zk-zorg-ai-goto-original-input-pos ()
  "Go to the position indicated by `zk-zorg-ai-original-input-pos'"
  (interactive)
  (unless zk-zorg-ai-original-input-pos
    (user-error "`zk-zorg-ai-original-input-pos was' was not set"))
  (switch-to-buffer (car zk-zorg-ai-original-input-pos))
  (goto-char (cdr zk-zorg-ai-original-input-pos)))

(defun zk-zorg-ai-use-current-entry-as-input ()
  "Use the entire current entry as the input for AI.  Sets
`zk-zorg-ai-original-input-pos'"
  (interactive)
  (let ((file (zk-zorg-ai-input-file-path))
        (output-file (zk-zorg-ai-output-file-path)))
    (zk-kill-buffer-visiting file)
    (save-mark-and-excursion
      (org-back-to-heading)
      (setq zk-zorg-ai-original-input-pos
            (cons (current-buffer) (point)))
      (org-mark-element)
      (write-region (region-beginning) (region-end) file))
    (find-file-other-window file)
    (when (and (file-exists-p output-file)
               (yes-or-no-p "AI output file exists, delete it?"))
      (zk-kill-buffer-visiting output-file)
      (delete-file output-file)
      (message "%s was deleted" output-file))))

(defun zk-zorg-ai-view-output ()
  "Display the AI's output buffer."
  (interactive)
  (let ((file (zk-zorg-ai-output-file-path)))
    (cond ((equal (buffer-file-name) file)
           (revert-buffer nil t))
          ((equal (buffer-file-name) (zk-zorg-ai-input-file-path))
           (zk-kill-buffer-visiting file)
           (find-file file))
          (t
           (zk-kill-buffer-visiting file)
           (find-file-other-window file)))))

(defvar zk-zorg-ai-num-recent-notes-files-for-context 5)

(defun zk-zorg-ai-generate-gemini-cli-prompts (&optional arg)
  "Generates a gemini-cli prompt for a selected task.  If the prefix arg is
present, it indicates the number of recent notes files that need to be
included in the context (default value is defined by
`zk-zorg-ai-num-recent-notes-files-for-context'"
  (interactive "P")
  (let* ((choice (read-char-choice
                  "Get prompt to: [s] sort notes; [t] generate TODO entries"
                  '(?s ?t)))
         (file-list (zk-zorg-list-note-files))
         (used-num-files (or arg zk-zorg-ai-num-recent-notes-files-for-context))
         (file-list-in-prompt
          (mapconcat (lambda (file) (concat "@" file))
                     (last file-list used-num-files)
                     " "))
         (prompt (cond ((eq choice ?s)
                        (format "Sort the meeting notes entry in @%s, considering %s.  Write the new meeting notes entry to a file named \"%s\". DO NOT modify any input file."
                                zk-zorg-ai-input-file-name
                                file-list-in-prompt
                                zk-zorg-ai-output-file-name))
                       ((eq choice ?t)
                        (format "Generate TODO entries for a meeting notes entry in @%s, considering %s.  Write the new entries to a file named \"%s\". DO NOT modify any input file."
                                zk-zorg-ai-input-file-name
                                file-list-in-prompt
                                zk-zorg-ai-output-file-name)))))
      (kill-new prompt)
      (message "Copied prompt: %s" prompt)))

;; Allow tag completion input (bound to TAB (C-i)) in minibuffers.
;; enable-recursive-minibuffers is needed because
;; zk-org-insert-tag-completion uses minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)
(define-key minibuffer-local-map (kbd "C-i") 'zk-org-insert-tag-completion)

(global-set-key (kbd "<f5>") 'zk-zorg-goto-latest-note-file)

(add-hook 'org-mode-hook 'zk-org-setup-bindings)
(add-hook 'org-mode-hook 'zk-org-set-file-encoding)
(add-hook 'org-agenda-mode-hook 'zk-org-setup-bindings)

(provide 'zorg)
