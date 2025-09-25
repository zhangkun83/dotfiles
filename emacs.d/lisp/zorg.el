(when (display-graphic-p)
  (setq leuven-scale-outline-headlines nil
        leuven-scale-org-agenda-structure nil)
  (load-theme 'leuven t))

(require 'zk)
(require 'zk-org)
(require 'org)
(require 'org-element)
(require 'org-tempo)
(require 'dash)
(require 'queue)

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
  (let* ((file-list (directory-files (zk-zorg-directory) nil "notes.*\\.org$"))
         ;; directory-files sort the files alphabeticaly
         (latest-file (car (last file-list))))
    (unless latest-file (user-error "No notes file found"))
    (let ((path (concat (zk-zorg-directory) "/" latest-file)))
    (switch-to-buffer
     (or (find-buffer-visiting path)
         (find-file-noselect path))))))

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
  (let* ((link-pair (zk-zorg-set-customid-and-get-headline-link-at-point nil))
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
               (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 "")))
    (message "Copied %sbacklink to this headline."
             (if mark-active "region with " ""))
    (deactivate-mark)))

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
generate one based on the text of the headline and set it.
Returns the CUSTOM_ID."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Not in org-mode"))
  (save-excursion
    (org-back-to-heading)
    (let* ((headline (org-element-at-point))
           (headline-text (substring-no-properties (org-get-heading t t t t)))
           (custom-id (or
                       (org-element-property :CUSTOM_ID headline)
                       (let ((new-id
                              (zk-org-generate-custom-id-from-text headline-text)))
                         (barf-if-buffer-read-only)
                         (org-set-property "CUSTOM_ID" new-id)
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

(defun zk-zorg-set-customid-and-get-headline-link-at-point (with-profile-name)
  "Returns a list of (link headline), where link is the external
link based on the CUSTOM_ID, and headline is the headline text.
Creates and sets the CUSTOM_ID if doesn't exist.  When
with-profile-name is non-nil, the link will include
zk-zorg-profile-name so that it can be used for scratch.el"
  (let ((return-value nil)
        (buffer (current-buffer)))
    (unless (eq major-mode 'org-mode)
        (user-error "Not in org-mode"))
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

(defun zk-org-add-note-to-logbook (content &optional show)
  "Add a note to the log book of the current entry.

If show is non-nil, will make the new note visible"
  (goto-char (org-log-beginning t))
  (save-excursion
    (org-insert-time-stamp nil t t "- Note taken on " " \\\\\n")
    (insert "  " content "\n"))
  (when show
    (org-show-context)))

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
  (org-tags-view arg (read-string "View for tags: " nil 'org-tags-history nil t)))

(defun zk-org-search-view (arg)
  "Like org-search-view but always create a new buffer for the
query."
  (interactive "P")
  (org-search-view arg (read-string "View for search term: " nil 'org-agenda-search-history nil t)))

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
  "Find and populate headings that have a `tbdsc' tag (to-be-discussed) and
also match another tag given by the user."
  (interactive)
  (let* ((all-tags (mapcar #'car (org-global-tags-completion-table)))
         (tag (completing-read
               "Populate agenda for tag: "
               all-tags
               nil
               t
               nil
               t))
         (links nil))
    (org-map-entries
     (lambda ()
       (push (zk-org-get-external-reference) links))
     (concat "tbdsc" "+" tag)
     'agenda)
    (dolist (link links)
      (insert "RE: " link)
      (newline 2))
    (message "Populated %d agenda items" (length links))))


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
  (local-set-key (kbd "C-c s") 'zk-org-search-view)
  (local-set-key (kbd "C-c q") 'zk-org-set-tags-command)
  (local-set-key (kbd "C-c l i") 'zk-zorg-set-customid-at-point)
  (local-set-key (kbd "C-c l a") 'zk-zorg-populate-agenda-command)
  (local-set-key (kbd "C-c l l") 'zk-org-copy-external-link)
  (local-set-key (kbd "C-c l r") 'zk-org-copy-external-reference)
  (local-set-key (kbd "C-c l w") 'zk-zorg-copy-region-with-link-to-heading)
  (local-set-key (kbd "C-c l f") 'zk-zorg-create-reference-tree-command-1level)
  (local-set-key (kbd "C-c l C-f") 'zk-zorg-create-reference-tree-command)
  (local-set-key (kbd "C-c l C-t") 'zk-zorg-create-reference-trees-for-tags-command)
  (local-set-key (kbd "C-c l s") 'zk-org-locate-in-scratch-task-queue)
  (local-set-key (kbd "C-c l C-s") 'zk-org-fill-scratch-task-queue)
  (local-set-key (kbd "C-c r s") 'zk-zorg-show-status)
  (local-set-key (kbd "C-c r u") 'zk-zorg-rsync-upload)
  (local-set-key (kbd "C-c r d") 'zk-zorg-rsync-download)
  (local-set-key (kbd "C-c r C-d") 'zk-zorg-rsync-diff)
  (local-set-key (kbd "C-c r o") 'zk-zorg-set-outdated)
  (local-set-key (kbd "C-c e h") 'zk-org-export-html-to-clipboard)
  (local-set-key (kbd "C-c o") 'zk-org-open-next-link)
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


;; Reference tree implementation

(defun zk-zorg-create-reference-tree-command-1level ()
  (interactive)
  (zk-zorg-create-reference-tree
   (zk-zorg-create-reference-tree--create-entry-alist-for-current-entry) 1))

(defun zk-zorg-create-reference-tree-command ()
  "Create a buffer to display the reference tree of the current heading
entry (the starting entry).  A reference tree is a tree of heading
entries where the children of a node are the entries that contains links
point back to that node.

If a heading will appear at most once in the reference tree, so that
there won't be any infinite loops in the resulting tree.

This is useful for exploring all the related entries, directly or
indirectly linking to the starting entry."
  (interactive)
  (let* ((all-tags (mapcar #'car (org-global-tags-completion-table)))
         (tag (completing-read
               "Reftree for this heading with (optional) tag: "
               all-tags
               nil
               t
               nil
               t)))
    (zk-zorg-create-reference-tree
     (zk-zorg-create-reference-tree--create-entry-alist-for-current-entry)
     nil (when (and tag (> (length tag) 0)) (list tag)))))

(defun zk-zorg-create-reference-trees-for-tags-command ()
  "Create a buffer to display the reference trees of all root entries
that match the given root-tags.  A root entry is an entry that doesn't back
refer (with \"RE:\") to any other entries."
  (interactive)
  (let* ((all-tags (mapcar #'car (org-global-tags-completion-table)))
         (tag (completing-read
               "Reftrees for all root entries with this tag: "
               all-tags
               nil
               t
               nil
               t)))
    (when (equal "" tag) (user-error "No tag entered."))
    (zk-zorg-create-reference-trees-for-tags (list tag) nil (list tag))))

(defun zk-zorg-create-reference-trees-for-tags (root-tags &optional max-level required-tags)
  "Create a buffer to display the reference trees of all root entries that
match the given root-tags.  A root entry is an entry that doesn't back
refer (with \"RE:\") to any other entries."
  (let* ((pr (make-progress-reporter "Reference trees for tags"))
         (destid-to-src-entry-mp-alist
          (progn
            (progress-reporter-force-update pr "creating index")
            (zk-zorg-create-reference-tree--create-index)))
         (destid-to-src-entry-mp
          (alist-get ':destid-to-src-entry-mp destid-to-src-entry-mp-alist))
         (root-entry-list
          (alist-get ':root-entry-list destid-to-src-entry-mp-alist))
         (output-buffer
          (zk-recreate-buffer
           (concat "*zorg tag reftrees* "
                   (if max-level (format "(max-level:%d) " max-level) "")
                   (concat " for (tags: " (mapconcat 'identity root-tags ":") ")")
                   (if required-tags
                       (concat " (required tags: " (mapconcat 'identity required-tags ":") ")")
                     "")))))
    (with-current-buffer output-buffer
      (org-mode))
    (dolist (root-entry root-entry-list)
      (when (zk-is-subset-p root-tags (alist-get ':tags root-entry))
        (let ((bfs-filtered-destid-to-src-entry-mp
               (progn
                 (progress-reporter-force-update
                  pr (concat "searching related entries for " (alist-get ':title root-entry)))
                 (zk-zorg-create-reference-tree--bfs-filter-destid-to-src-entry-map
                  root-entry required-tags destid-to-src-entry-mp))))
          (progress-reporter-force-update
           pr (concat "generating output for " (alist-get ':title root-entry)))
          (zk-zorg-create-reference-tree--create-subtree-for-entry
           0
           max-level
           root-entry
           bfs-filtered-destid-to-src-entry-mp
           output-buffer
           (make-hash-table :test 'equal))
          (with-current-buffer output-buffer
            (newline)))))
    (switch-to-buffer output-buffer)
    (zk-zorg-create-reference-tree--bind-keys
     (list 'zk-zorg-create-reference-trees-for-tags
           `(quote ,root-tags) max-level `(quote ,required-tags)))
    (newline)
    (insert "(Back-reference trees, where children entries contain links "
            "pointing to the parent entry.")
    (org-fill-paragraph)
    (goto-char 0)
    (set-buffer-modified-p nil)
    (progress-reporter-done pr)
    (read-only-mode t)))

(defvar-local zk-zorg-create-reference-tree-refresh-form nil
  "The form to be evaluated to refresh the ref tree buffer")

(defun zk-zorg-create-reference-tree--bind-keys (refresh-form)
  ;; Copy the key map to prevent from unintentionally modifying the
  ;; shared org-mode-map
  (let ((my-local-map (copy-keymap (current-local-map))))
    (use-local-map my-local-map)
    (define-key my-local-map (kbd "RET") 'zk-org-open-next-link)
    (setq zk-zorg-create-reference-tree-refresh-form refresh-form)
    (define-key my-local-map (kbd "g") 'zk-zorg-create-reference-tree--refresh)))

(defun zk-zorg-create-reference-tree--refresh ()
  (interactive)
  (eval zk-zorg-create-reference-tree-refresh-form))

(defun zk-zorg-create-reference-tree (entry-alist &optional max-level required-tags)
  "Create a buffer to display the reference tree of the given
entry (the starting entry).  A reference tree is a tree of heading
entries where the children of a node are the entries that contains links
point back to that node.

If a heading will appear at most once in the reference tree, so that
there won't be any infinite loops in the resulting tree.

This is useful for exploring all the related entries, directly or
indirectly linking to the starting entry.

If an list of tags is provided to the optional required-tags argument,
this function will expand the search from an entry only when the entry
is tagged with all of the tags."
  (let* ((pr (make-progress-reporter "Reference tree"))
         (destid-to-src-entry-mp
          (progn
            (progress-reporter-force-update pr "creating index")
            (alist-get ':destid-to-src-entry-mp
                       (zk-zorg-create-reference-tree--create-index))))
         (output-buffer
          (zk-recreate-buffer
           (concat "*zorg reftree* "
                   (if max-level (format "(max-level:%d) " max-level) "")
                   (alist-get ':title entry-alist)
                   (if required-tags
                       (concat " (" (mapconcat 'identity required-tags ":") ")")
                     ""))))
         (bfs-filtered-destid-to-src-entry-mp
          (progn
            (progress-reporter-force-update pr "searching related entries")
            (zk-zorg-create-reference-tree--bfs-filter-destid-to-src-entry-map
             entry-alist required-tags destid-to-src-entry-mp))))
    (with-current-buffer output-buffer
      (org-mode))
    (progress-reporter-force-update pr "generating output")
    (zk-zorg-create-reference-tree--create-subtree-for-entry
     0
     max-level
     entry-alist
     bfs-filtered-destid-to-src-entry-mp
     output-buffer
     (make-hash-table :test 'equal))
    (switch-to-buffer output-buffer)
    (zk-zorg-create-reference-tree--bind-keys
     (list 'zk-zorg-create-reference-tree
           `(quote ,entry-alist)
           max-level
           `(quote ,required-tags)))
    (newline)
    (insert "(Back-reference tree, where children entries contain links "
            "pointing to the parent entry.)")
    (org-fill-paragraph)
    (goto-char 0)
    (set-buffer-modified-p nil)
    (progress-reporter-done pr)
    (read-only-mode t)))

(defun zk-zorg-create-reference-tree--bfs-filter-destid-to-src-entry-map
    (start-entry-alist
     required-tags
     destid-to-src-entry-mp)
  "Using the given destid-to-src-entry-mp multimap created by
zk-zorg-create-reference-tree--create-index, do a
BFS traverse from the given start-entry-alist, until all entry links are
visited.  Record the used destid-to-src-entry edges during the traverse in
a new multimap and return it."
  (let ((bfs-queue (make-queue))
        (visited-entry-links-hash-set (make-hash-table :test 'equal))
        (filtered-mp (make-hash-table :test 'equal)))
    (queue-enqueue bfs-queue start-entry-alist)
    (while (not (queue-empty bfs-queue))
      (let* ((entry-alist (queue-dequeue bfs-queue))
             (link (alist-get ':link entry-alist))
             (tags (alist-get ':tags entry-alist))
             (custom-id (alist-get ':custom-id entry-alist)))
        ;; Although we record srclink to visited-entry-links-hash-set below,
        ;; this is necessary for the very first entry-alist.
        (puthash link t visited-entry-links-hash-set)
        (when (and custom-id (zk-is-subset-p required-tags tags))
          (dolist (src-entry-alist (zk-multimap-get destid-to-src-entry-mp custom-id))
            (let ((srclink (alist-get ':link src-entry-alist)))
              (unless (gethash srclink visited-entry-links-hash-set)
                (puthash srclink t visited-entry-links-hash-set)
                (zk-multimap-add filtered-mp custom-id src-entry-alist)
                (queue-enqueue bfs-queue src-entry-alist)))))))
    filtered-mp))

(defun zk-zorg-create-reference-tree--create-subtree-for-entry
    (level
     max-level
     entry-alist
     destid-to-src-entry-mp
     output-buffer
     visited-entry-links-hash-set)
  (let ((todo-keyword (alist-get ':todo-keyword entry-alist))
        (link (alist-get ':link entry-alist))
        (tags (alist-get ':tags entry-alist))
        (custom-id (alist-get ':custom-id entry-alist)))
    (unless (gethash link visited-entry-links-hash-set)
      (puthash link t visited-entry-links-hash-set)
      (with-current-buffer output-buffer
        (insert (make-string (* 2 level) ?\ ) (if (= 0 level) "+ " "- ")
                (if todo-keyword (concat "*" todo-keyword "* ") "")
                (alist-get ':title entry-alist)
                " (" (alist-get ':file entry-alist) ")"
                (if tags (concat " /" (mapconcat 'identity tags ":") "/") "")
                " [[" link "][^]]")
        (newline))
      (when (and custom-id
                 (not (and max-level (>= level max-level))))
        (dolist (src-entry-alist (zk-multimap-get destid-to-src-entry-mp custom-id))
          ;; Recursively create the subtree for this entry
          (zk-zorg-create-reference-tree--create-subtree-for-entry
           (+ level 1)
           max-level
           src-entry-alist
           destid-to-src-entry-mp
           output-buffer
           visited-entry-links-hash-set))))))

(defun zk-zorg-create-reference-tree--create-entry-alist-for-current-entry ()
  "Create an alist for the current heading, which contains keys (:link :todo-keyword :title :file :tags :custom-id)."
  (save-excursion
    (org-back-to-heading)
    (let* ((element (or (org-element-at-point)
                        (error "No heading found")))
           (heading-link (zk-zorg-create-reference-tree--get-current-heading-link))
           (custom-id (zk-org-get-customid-at-point))
           (file (file-name-nondirectory (buffer-file-name)))
           (todo-keyword (org-element-property :todo-keyword element))
           (tags (org-element-property :tags element))
           (title (zk-org-neutralize-timestamp (org-element-property :title element))))
      (list (cons ':link heading-link)
            (cons ':todo-keyword todo-keyword)
            (cons ':title title)
            (cons ':file file)
            (cons ':tags tags)
            (cons ':custom-id custom-id)))))

(defun zk-zorg-create-reference-tree--create-index ()
  "Scan the whole repo and create the index for creating reference trees.

It contains a multimap where the key are entry IDs (CUSTOM_ID), and the
values are alists (:link :todo-keyword :title :file :tags) of the
heading entries that contain references to the key ID.

It also contains a list that contains the alists of entries that don't
contain any back references (with \"RE:\").  Those are considered as root entries.

The return value is an alist (:destid-to-src-entry-mp :root-entry-list).
"
  (let ((id-to-link-multimap (make-hash-table :test 'equal))
        (root-entry-list nil))
    (org-map-entries
     (lambda ()
       (let* ((entry-alist
               (zk-zorg-create-reference-tree--create-entry-alist-for-current-entry))
              ;; The links are in the format
              ;; "[[file:notes2024q1.org::#ramp_up_on_mitigation_engine_work][link]]"
              ;; where "ramp_up_on_mitigation_engine_work" is the ID to
              ;; be extracted.  The CUSTOM_ID is generated by
              ;; zk-org-generate-link.
              (link-regex "\\[\\[file:[^][:#]*::#\\([^][:#]*\\)\\]\\[\\([^][:#]*\\)\\]")
              ;; The back references are in the format
              ;; "RE: Foo bar [[link][text]]".  There may be at most 3 newlines between
              ;; the "RE: " and the link.
              (backref-link-regex
               (concat "RE: .*\\(?:\n.*\\)\\{0,3\\}" link-regex))
              (is-root-entry-p t))
         (save-mark-and-excursion
           (zk-org-mark-heading-content)
           (save-mark-and-excursion
             ;; Search for CUSTOM_ID references
             (while (re-search-forward link-regex (region-end) t)
               (let ((dest-id (match-string-no-properties 1))
                     ;; link-text is not used for now, but may be used in the future
                     (link-text (match-string-no-properties 2)))
                 ;; Ignore forward-reference links created by
                 ;; zk-zorg-copy-region-with-link-to-heading
                 (zk-multimap-add id-to-link-multimap
                                  dest-id
                                  ;; Alist of the source entry
                                  entry-alist))))
           (save-mark-and-excursion
             ;; Search for any ack reference
             (when (re-search-forward backref-link-regex (region-end) t)
               (setq is-root-entry-p nil))))
         (when is-root-entry-p
           (push entry-alist root-entry-list))))
     t
     'agenda-with-archives)
    (list (cons ':destid-to-src-entry-mp id-to-link-multimap)
          (cons ':root-entry-list (reverse root-entry-list)))))

(defun zk-zorg-create-reference-tree--get-current-heading-link ()
  (or (zk-org-get-current-heading-link)
      (error "Failed to get heading link at pos %d of %s"
                  (point) (buffer-file-name))))

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
