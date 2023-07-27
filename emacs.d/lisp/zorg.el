(require 'zk)
(require 'org)
(require 'org-element)
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

(defun zk-zorg-directory ()
  "Returns the absolute directory for local org files"
  (concat zk-user-home-dir "/" zk-zorg-profile-name))

;; Possible values: outdated, downloading, uploading, clean, modified
(setq zk-zorg-status 'outdated)

;; Most values are copied from the default mode-line-format. I added
;; zk-zorg-status.
(setq-default
 mode-line-format
 '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
   "  " mode-line-modes
   (:eval (format "[zorg: %s]" zk-zorg-status))
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
    (switch-to-buffer (find-file-noselect (concat (zk-zorg-directory) "/" latest-file)))))

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
  
(defun zk-org-set-generated-custom-id-and-copy-external-link (&optional arg)
  "If CUSTOM_ID of the current org headline doesn't exist,
generate one based on the text of the headline and set it.  Copy
the external link based on the CUSTOM_ID to the kill ring.  When
called with the prefix argument, the link will include
zk-zorg-profile-name so that it can be used for scratch.el"
  (interactive "P")
  (let* ((link-pair (zk-org-get-headline-link-at-point arg))
         (link (nth 0 link-pair))
         (headline-text (nth 1 link-pair)))
    (kill-new link)
    (message "Copied \"%s\"" link)))

(defun zk-org-set-generated-custom-id-and-copy-external-reference (&optional arg)
  "If CUSTOM_ID of the current org headline doesn't exist,
generate one based on the text of the headline and set it.  Copy
a reference, with the headline string followed by a link based on
the CUSTOM_ID, to the kill ring.  When called with the prefix
argument, the link will include zk-zorg-profile-name so that it
can be used for scratch.el"
  (interactive "P")
  (let (reference (zk-org-get-external-reference arg))
    (kill-new reference)
    (message "Copied \"%s\"" reference)))

(defun zk-org-get-external-reference (&optional arg)
  "If CUSTOM_ID of the current org headline doesn't exist,
generate one based on the text of the headline and set it.
Returns a reference, with the headline string followed by a link
based on the CUSTOM_ID.  When called with the prefix argument,
the link will include zk-zorg-profile-name so that it can be used
for scratch.el"
  (let* ((link-pair (zk-org-get-headline-link-at-point arg))
         (link (nth 0 link-pair))
         (headline-text (nth 1 link-pair))
         (reference (format "%s ([[%s][link]])" headline-text link)))
    reference))

(defun zk-org-get-headline-link-at-point (with-profile-name)
  "If CUSTOM_ID of the current org headline doesn't exist,
generate one based on the text of the headline and set it.
Returns a list of (link headline), where link is the external
link based on the CUSTOM_ID, and headline is the headline text.
When with-profile-name is non-nil, the link will include
zk-zorg-profile-name so that it can be used for scratch.el"
  (let ((return-value nil))
    (save-excursion
      ;; Allows invoking this command directly from the agenda buffer
      (if (eq major-mode 'org-agenda-mode)
          (org-agenda-switch-to))
      (zk-org-move-to-current-heading)
      (let* ((headline (org-element-at-point))
             (headline-text (substring-no-properties (org-get-heading t t t t)))
             (custom-id (or
                         (org-element-property :CUSTOM_ID headline)
                         (let ((new-id
                                (zk-org-generate-custom-id-from-text headline-text)))
                           (org-set-property "CUSTOM_ID" new-id)
                           new-id)))
             (link (concat "file:"
                           (if with-profile-name (concat "@" zk-zorg-profile-name ":"))
                           (file-name-nondirectory (buffer-file-name (current-buffer)))
                           "::#"
                           custom-id)))
        (setq return-value (list link headline-text))))
    ;; save-excursion will restore the previous buffer as current, but
    ;; it doesn't switch the current window to that buffer.  We need
    ;; to manually do it.
    (switch-to-buffer (current-buffer))
    return-value))

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
      (org-link-open-from-string link)
      (zk-org-add-note-to-logbook
       (concat "Referenced in: " back-ref) t))))

(defun zk-org-move-to-current-heading ()
  "Move to the current heading if not already at a heading."
  (interactive)
  (unless (eq 'headline (org-element-type (org-element-at-point)))
    (org-previous-visible-heading 1)))

(defun zk-org-tags-view (arg)
  "org-tags-view will always ask for the tags before switching to
an existing view buffer if available, but it doesn't use the
entered tags anyway if org-agenda-sticky is turned
on. zk-org-tags-view will try to switch to the existing buffer
without asking."
  (interactive "P")
  (let* ((view-buffer-name (if arg "*Org Agenda(M)*" "*Org Agenda(m)*"))
         (view-buffer (get-buffer view-buffer-name)))
    (if view-buffer
        (switch-to-buffer view-buffer)
      (org-tags-view arg))))

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
  (local-set-key (kbd "C-c m") 'zk-org-tags-view)
  (local-set-key (kbd "C-c s") 'org-search-view)
  (local-set-key (kbd "C-c q") 'zk-org-set-tags-command)
  (local-set-key (kbd "C-c g n") 'zk-zorg-goto-latest-note-file)
  (local-set-key (kbd "C-c l l") 'zk-org-set-generated-custom-id-and-copy-external-link)
  (local-set-key (kbd "C-c l r") 'zk-org-set-generated-custom-id-and-copy-external-reference)
  (local-set-key (kbd "C-c l b") 'zk-org-log-backlink-at-point)
  (local-set-key (kbd "C-c r s") 'zk-zorg-show-status)
  (local-set-key (kbd "C-c r u") 'zk-zorg-rsync-upload))

(defun zk-org-set-file-encoding ()
  ;; Force unix newline format, even on Windows
  (setq buffer-file-coding-system 'utf-8-unix))

(defun zk-zorg-startup-init ()
  "Initializes zorg session."
  (unless zk-zorg-rsync-backup-dir
    (user-error "zk-zorg-rsync-backup-dir not set"))
  (unless zk-zorg-profile-name
    (user-error "zk-zorg-profile-name not set"))
  (unless (eq zk-zorg-status 'outdated)
    (user-error "Unexpected zorg status: %s" zk-zorg-status))
  (setq org-agenda-files (list (zk-zorg-directory))
        frame-title-format zk-zorg-profile-name)
  (setq zk-zorg-status 'downloading)
  (let ((default-directory (zk-zorg-directory)))
    (switch-to-buffer zk-zorg-rsync-buffer-name)
    (erase-buffer)
    (insert "Downloading remote files ...\n")
    (if (eq 0 (call-process "rsync" nil zk-zorg-rsync-buffer-name t
                            "-rtuv" (concat zk-zorg-rsync-backup-dir "/") "."))
        (progn
          (setq zk-zorg-status 'clean)
          (read-string "Download successful. Press Enter to continue ...")
          (kill-buffer)
          (zk-zorg-startup-open nil))
      (setq zk-zorg-status 'outdated)
      (if (y-or-n-p "Download failed. Press y to retry, n to open in read-only mode")
          (zk-zorg-startup-init)
        (kill-buffer)
        (zk-zorg-startup-open t)))))

(defun zk-zorg-rsync-upload ()
  (interactive)
  (unless (or
           (eq zk-zorg-status 'clean)
           (eq zk-zorg-status 'modified))
    (user-error "Unexpected zorg status: %s" zk-zorg-status))
  (setq zk-zorg-status 'uploading)
  (let ((default-directory (zk-zorg-directory)))
    (switch-to-buffer zk-zorg-rsync-buffer-name)
    (erase-buffer)
    (insert "Uploading local changes ...\n")
    (if (eq 0 (call-process "rsync" nil zk-zorg-rsync-buffer-name t
                            "-rtuv"
                            (concat "--files-from=" (zk-zorg-generate-upload-list-file))
                            "./" zk-zorg-rsync-backup-dir))
        (progn
          (setq zk-zorg-status 'clean)
          (read-string "Upload successful. Press Enter to continue ...")
          (kill-buffer))
      (setq zk-zorg-status 'modified)
      (if (y-or-n-p "Upload failed. Retry?")
          (zk-zorg-rsync-upload)))))

(defun zk-zorg-rsync-check-remote-freshness ()
  "Called right after the initial download to make sure the remote
is as fresh as the local copy.  Returns t if check passes, nil if
check failed."
  (let ((default-directory (zk-zorg-directory)))
    (switch-to-buffer zk-zorg-rsync-buffer-name)
    (erase-buffer)
    (insert "Checking remote freshness ...\n")
    (if (eq 0 (call-process "rsync" nil zk-zorg-rsync-buffer-name t
                            "-ncrti"
                            (concat "--files-from=" (zk-zorg-generate-upload-list-file))
                            "./" zk-zorg-rsync-backup-dir))
        (with-current-buffer zk-zorg-rsync-buffer-name
          (if (> (count-lines (point-min) (point-max)) 1)
              (progn
                (read-string "WARNING: local files differ from remote.  Please check ...")
                nil)
            (read-string "Local files are consistent with remote.  Press Enter to continue ...")
            (kill-buffer)
            t))
      (if (y-or-n-p "Failed to check remote freshness. Retry?")
          (zk-zorg-rsync-check-remote-freshness)
        nil))))

(defun zk-zorg-startup-open (readonly)
  (when
      (if readonly
          (add-hook 'org-mode-hook (lambda() (read-only-mode 1)))
        (zk-zorg-rsync-check-remote-freshness))
    (when zk-zorg-startup-view-func
      (funcall zk-zorg-startup-view-func))
    (setq server-name zk-zorg-profile-name)
    (server-start)
    (message "Ready%s. Have a very safe and productive day!"
             (if readonly " (read-only)" ""))))

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

(add-hook 'org-mode-hook 'zk-org-setup-bindings)
(add-hook 'org-mode-hook 'zk-org-set-file-encoding)
(add-hook 'org-agenda-mode-hook 'zk-org-setup-bindings)

(setq confirm-kill-emacs 'zk-zorg-shutdown-confirm)


(provide 'zorg)
