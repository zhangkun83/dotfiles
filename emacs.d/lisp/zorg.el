(require 'zk)
(require 'org)
(require 'org-element)
(require 'org-tempo)
(require 'dash)

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
    (with-current-buffer (find-file-noselect list-file)
      (erase-buffer)
      (dolist (file file-list)
        (when (string-match-p "^[^.#].+\\.\\(org\\)\\|\\(org_archive\\)$" file)
          (insert file "\n")))
      (save-buffer)
      (kill-buffer))
    list-file-name))

(defun zk-zorg-goto-latest-note-file ()
  "Go to the latest note org file under the same directory."
  (interactive)
  (let* ((file-list (directory-files (zk-zorg-directory) nil "notes.*\\.org"))
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
         (downcase
          (replace-regexp-in-string
           "\\`_*" ""  ; remove leading underscores
           (replace-regexp-in-string
            "_*\\'" ""  ; remove trailing underscores
            (replace-regexp-in-string  ; replace non alphanumerics to underscores
             "[^a-zA-Z0-9_]+" "_" text))))))
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
  (let* ((link-pair (zk-org-get-headline-link-at-point arg))
         (link (nth 0 link-pair))
         (link-with-text (format "([[%s][link]])" link)))
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
  (let* ((link-pair (zk-org-get-headline-link-at-point arg))
         (link (nth 0 link-pair))
         (headline-text (nth 1 link-pair))
         (reference (format "%s ([[%s][link]])" headline-text link)))
    reference))

(defun zk-org-insert-external-reference-to-scratch-task-queue ()
  "Insert the external reference of the current heading to the task
queue of the scratch server, and set the priority to A."
  (interactive)
  (let ((content (zk-org-get-external-reference t)))
    (server-eval-at
     "scratch"
     (list 'progn
           (list 'zk-scratch-insert-to-task-queue content)
         '(raise-frame)
         (list 'message "Added to queue: %s" content)))
    (org-priority ?A)
    (message "Sent reference to scratch.  Priority set to A.")))

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

(defun zk-org-generate-custom-id-at-point ()
  "If CUSTOM_ID of the current org headline doesn't exist,
generate one based on the text of the headline and set it."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Not in org-mode"))
  (save-excursion
    (zk-org-move-to-current-heading)
    (let* ((headline (org-element-at-point))
           (headline-text (substring-no-properties (org-get-heading t t t t)))
           (custom-id (or
                       (org-element-property :CUSTOM_ID headline)
                       (let ((new-id
                              (zk-org-generate-custom-id-from-text headline-text)))
                         (org-set-property "CUSTOM_ID" new-id)
                         new-id))))
      (message "CUSTOM_ID: %s" custom-id))))

(defun zk-org-get-headline-link-at-point (with-profile-name)
  "Returns a list of (link headline), where link is the external
link based on the CUSTOM_ID, and headline is the headline text.
When with-profile-name is non-nil, the link will include
zk-zorg-profile-name so that it can be used for scratch.el"
  (let ((return-value nil)
        (buffer (current-buffer)))
    (unless (eq major-mode 'org-mode)
        (user-error "Not in org-mode"))
    (save-excursion
      (zk-org-move-to-current-heading)
      (let* ((headline (org-element-at-point))
             (headline-text (substring-no-properties (org-get-heading t t t t)))
             (custom-id (or (org-element-property :CUSTOM_ID headline)
                            (user-error "This entry doesn't have a CUSTOM_ID")))
             (link (concat "file:"
                           (if with-profile-name (concat "@" zk-zorg-profile-name ":"))
                           (file-name-nondirectory (buffer-file-name (zk-get-base-buffer (current-buffer))))
                           "::#"
                           custom-id)))
        (setq return-value (list link (zk-org-neutralize-timestamp headline-text)))))
    return-value))

(defun zk-org-get-customid-at-point ()
  "Returns the CUSTOM_ID of the current org entry.  nil if CUSTOM_ID
is not there."
  (save-excursion
    (zk-org-move-to-current-heading)
    (org-element-property :CUSTOM_ID (org-element-at-point))))

(defun zk-org-find-references-to-current-entry ()
  "Search for references to the current entry using the
 CUSTOM_ID."
  (interactive)
  (let ((custom-id (zk-org-get-customid-at-point)))
    (unless custom-id
      (user-error "This entry doesn't have a CUSTOM_ID, thus it won't have any reference."))
    (org-search-view nil custom-id)))

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

(defun zk-org-log-backlink-at-point ()
  "If the point is on a link, record a log in the linked entry
back to the current entry."
  (interactive)
  (let ((link (zk-org-get-link-at-point)))
    ;; Only works on a org entry link, in the format of
    ;; "(file:filename.org::)?#nodeid"
    (unless (and link (string-match-p "^\\(file:[^:]+::\\)?#[^#]+$" link))
      (user-error "Not a link to an org entry: %s" link))
    (let ((back-ref (zk-org-get-external-reference)))
      (org-open-at-point)
      (zk-org-add-note-to-logbook
       (concat "Referenced in: " back-ref) t))))

(defun zk-org-move-to-current-heading ()
  "Move to the current heading if not already at a heading."
  (interactive)
  (unless (eq 'headline (org-element-type (org-element-at-point)))
    (org-previous-visible-heading 1)))

(defun zk-org-insert-tag-completion ()
  "Insert a tag to the current buffer with completion"
  (interactive)
  (insert (completing-read "Insert tag: " (org-global-tags-completion-table))))

(defun zk-org-tags-view (arg)
  "org-tags-view will always ask for the tags before switching to
an existing view buffer if available, but it doesn't use the
entered tags anyway if org-agenda-sticky is turned
on. zk-org-tags-view will always create a new buffer for the query."
  (interactive "P")
  (org-tags-view arg (read-string "Match: " nil 'org-tags-history nil t)))

(defun zk-org-search-view (arg)
  "Like org-search-view but always create a new buffer for the
query."
  (interactive "P")
  (org-search-view arg (read-string "Search: " nil 'org-agenda-search-history nil t)))

(defun zk-org-set-tags-command ()
  "Set tags to the current entry. It's better than
org-set-tags-command in that it uses the agenda files instead of
the current file for completion."
  (interactive)
  (let* ((current-tags (org-get-tags nil t))
         (new-tag (completing-read
                   (concat "Tags: " (org-make-tag-string current-tags))
                   (org-global-tags-completion-table)
                   nil
                   nil
                   nil
                   t)))
    (org-toggle-tag new-tag 'on)))

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
  (message "zorg status: %s" zk-zorg-status))

(defun zk-org-setup-bindings ()
  "Register my own shortcuts for org mode"
  (local-set-key (kbd "C-c a") 'org-agenda-list)
  (local-set-key (kbd "C-c t") 'org-todo-list)
  (local-set-key (kbd "C-c m") 'zk-org-tags-view)
  (local-set-key (kbd "C-c s") 'zk-org-search-view)
  (local-set-key (kbd "C-c q") 'zk-org-set-tags-command)
  (local-set-key (kbd "C-c l i") 'zk-org-generate-custom-id-at-point)
  (local-set-key (kbd "C-c l l") 'zk-org-copy-external-link)
  (local-set-key (kbd "C-c l r") 'zk-org-copy-external-reference)
  (local-set-key (kbd "C-c l b") 'zk-org-log-backlink-at-point)
  (local-set-key (kbd "C-c l f") 'zk-org-find-references-to-current-entry)
  (local-set-key (kbd "C-c l s") 'zk-org-insert-external-reference-to-scratch-task-queue)
  (local-set-key (kbd "C-c r s") 'zk-zorg-show-status)
  (local-set-key (kbd "C-c r u") 'zk-zorg-rsync-upload)
  (local-set-key (kbd "C-c r d") 'zk-zorg-rsync-download)
  (local-set-key (kbd "C-c r C-d") 'zk-zorg-rsync-diff)
  (local-set-key (kbd "C-c r o") 'zk-zorg-set-outdated)
  (local-set-key (kbd "C-c c") 'zk-org-clone-narrowed-buffer))

(defun zk-org-set-file-encoding ()
  ;; Force unix newline format, even on Windows
  (setq buffer-file-coding-system 'utf-8-unix))

(defun zk-zorg-startup-init ()
  "Initializes zorg session."
  (unless zk-zorg-rsync-backup-dir
    (user-error "zk-zorg-rsync-backup-dir not set"))
  (unless zk-zorg-profile-name
    (user-error "zk-zorg-profile-name not set"))
  (setq default-directory (zk-zorg-directory))
  (setq org-agenda-files (list (zk-zorg-directory))
        zk-frame-title-base-name zk-zorg-profile-name)
  (setq org-agenda-span 'fortnight)
  (setq org-agenda-show-all-dates t)
  (setq org-deadline-warning-days 180)
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
    (user-error "Cannot download when zorg status is %s" zk-zorg-status))
  (setq zk-zorg-status 'downloading)
  (let ((default-directory (zk-zorg-directory)))
    (switch-to-buffer zk-zorg-rsync-buffer-name)
    (erase-buffer)
    (insert "Downloading remote files ...\n")
    (if (eq 0 (call-process "rsync" nil zk-zorg-rsync-buffer-name t
                            "-rtuv" (concat zk-zorg-rsync-backup-dir "/") "."))
        (progn
          (read-string "Download successful. Press Enter to continue to check remote freshness ...")
          (if (zk-zorg-rsync-check-remote-freshness)
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
      (read-string "Download failed.  Press Enter to continue ...")
      (kill-buffer))))

(defun zk-zorg-rsync-upload ()
  (interactive)
  (when (zk-has-unsaved-files-p)
    (user-error "There are unsaved files."))
  (unless (or
           (eq zk-zorg-status 'clean)
           (eq zk-zorg-status 'modified))
    (user-error "Cannot upload when zorg status is %s" zk-zorg-status))
  (let ((default-directory (zk-zorg-directory)))
    (setq zk-zorg-status 'uploading)
    (switch-to-buffer zk-zorg-rsync-buffer-name)
    (erase-buffer)
    (insert "Uploading local changes ...\n")
    (if (eq 0 (call-process "rsync" nil zk-zorg-rsync-buffer-name t
                            "-rtuv"
                            (concat "--files-from=" (zk-zorg-generate-upload-list-file))
                            "./" zk-zorg-rsync-backup-dir))
        (progn
          (setq zk-zorg-status 'clean)
          (read-string "Upload successful. Press Enter to continue ..."))
      (setq zk-zorg-status 'modified)
      (read-string "Upload failed. Press Enter to continue ..."))
    (kill-buffer)))

(defun zk-zorg-rsync-diff ()
  "Display the diff of the local files against the remote files."
  (interactive)
  (when (zk-has-unsaved-files-p)
    (user-error "There are unsaved files."))
  (let ((temp-directory (concat (zk-zorg-directory) ".tmp-diff-remote"))
        (output-buffer zk-zorg-rsync-diff-buffer-name))
    (mkdir temp-directory t)
    (let ((default-directory temp-directory))
      (switch-to-buffer output-buffer)
      (erase-buffer)
      (insert "Downloading remote files for diff'ing ...\n")
      (unless (eq 0 (call-process "rsync" nil output-buffer t
                                  "-crti" "--delete" (concat zk-zorg-rsync-backup-dir "/") "."))
        (error "Failed to download remote files"))
      (insert "Generating diff ...\n"))
    (call-process "diff" nil output-buffer t
                  "-r" temp-directory (zk-zorg-directory))
    (insert "End of diff.\n")))

(defun zk-zorg-rsync-check-remote-freshness ()
  "Called right after the initial download to make sure the remote
is as fresh as the local copy.  Returns t if check passes, nil if
check failed."
  (let ((default-directory (zk-zorg-directory))
        (do-it-p t)
        (consistent-p nil))
    (while do-it-p
      (switch-to-buffer zk-zorg-rsync-buffer-name)
      (erase-buffer)
      (insert "Checking remote freshness ...\n")
      (if (eq 0 (call-process "rsync" nil zk-zorg-rsync-buffer-name t
                              "-ncrti"
                              (concat "--files-from=" (zk-zorg-generate-upload-list-file))
                              "./" zk-zorg-rsync-backup-dir))
          (with-current-buffer zk-zorg-rsync-buffer-name
            (setq do-it-p nil)
            (if (> (count-lines (point-min) (point-max)) 1)
                (read-string "WARNING: local files differ from remote.  Please check ...")
              (read-string "Local files are consistent with remote.  Press Enter to continue ...")
              (kill-buffer)
              (setq consistent-p t)))
        (unless (y-or-n-p "Failed to check remote freshness. Retry?")
          (setq do-it-p nil))))
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
  (message "zorg is now outdated."))

(defun zk-zorg-startup-open ()
  (when (or (eq zk-zorg-status 'clean)
            (eq zk-zorg-status 'outdated))
    (when zk-zorg-startup-view-func
      (funcall zk-zorg-startup-view-func))
    (setq server-name zk-zorg-profile-name)
    (server-start)
    (message "Ready (%s). Have a very safe and productive day!"
             zk-zorg-status)))

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

(defun zk-zorg-frame-title-frame-name-function ()
  (or (and (not (buffer-base-buffer))  ; Keep the default title for
                                       ; indirect buffers
           (eq major-mode 'org-mode)
           (concat (zk-org-get-heading-string)
                   " (" (zk-frame-title-frame-name-default-function) ")"))
      (zk-frame-title-frame-name-default-function)))

(setq zk-frame-title-frame-name-function
      'zk-zorg-frame-title-frame-name-function)

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

(setq confirm-kill-emacs 'zk-zorg-shutdown-confirm)

(when (display-graphic-p)
  (setq leuven-scale-outline-headlines nil
        leuven-scale-org-agenda-structure nil)
  (load-theme 'leuven t))

(provide 'zorg)
