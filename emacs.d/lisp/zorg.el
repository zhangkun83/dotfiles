(require 'zk)
(require 'zk-org)
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
         (reference (format "%s %s[[%s][^]]"
                            headline-text
                            (if arg (concat "#" zk-zorg-profile-name) "")
                            link)))
    reference))

(defun zk-org-copy-region-with-backlink ()
  "Copy the content of the current active region, with a
backlink to the current headline.  The content of the headline
won't be included, but the first timestamp will be included.

This is useful for copying contents from a note entry to a task."
  (interactive)
  (unless mark-active
    (user-error "Region not active"))
  (let* ((link-pair (zk-org-get-headline-link-at-point nil))
         (link (nth 0 link-pair))
         (headline-text (nth 1 link-pair))
         (backlink (format "([[%s][%s]])"
                           link
                           (if (string-match
                                ;; The "<>" and "[]" have been
                                ;; converted to "()" by
                                ;; zk-org-get-headline-link-at-point using
                                ;; zk-org-neutralize-timestamp
                                "(\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][^)]*\\))"
                                headline-text)
                               (match-string 1 headline-text)
                             "ref"))))
    (kill-new (concat backlink
                      "\n"
                      (buffer-substring (region-beginning) (region-end))))
    (message "Copied region with backlink to this headline.")
    (deactivate-mark)))

(defun zk-org-get-scratch-reference-metadata ()
  "Returns a list of two elements representing an external reference
to be used in the scratch.  The first element is the text content
and the second element is the reference ID."
  (let* ((content (zk-org-get-external-reference t))
         (link-pair (zk-org-get-headline-link-at-point t))
         (id (nth 0 link-pair)))
    (list content id)))

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
                              ((eq priority ?A) "Current priorities"))))
                   (when category
                     (zk-org-generate-custom-id-at-point)
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

(defun zk-org-generate-custom-id-at-point ()
  "If CUSTOM_ID of the current org headline doesn't exist,
generate one based on the text of the headline and set it.
Returns the CUSTOM_ID."
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
                         (barf-if-buffer-read-only)
                         (org-set-property "CUSTOM_ID" new-id)
                         new-id))))
      (when (called-interactively-p 'any)
        (message "CUSTOM_ID: %s" custom-id))
      custom-id)))

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
             (custom-id (zk-org-generate-custom-id-at-point))
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
  (org-tags-view arg (read-string "Match (TAB to search tag): " nil 'org-tags-history nil t)))

(defun zk-org-search-view (arg)
  "Like org-search-view but always create a new buffer for the
query."
  (interactive "P")
  (org-search-view arg (read-string "Search (TAB to search tag): " nil 'org-agenda-search-history nil t)))

(defun zk-org-set-tags-command ()
  "Set tags to the current entry. It's better than
org-set-tags-command in that it uses the agenda files instead of
the current file for completion.  It keeps asking for new tag until
an empty line is entered."
  (interactive)
  (let ((new-tag nil))
    (while (not (equal new-tag ""))
      (setq new-tag (completing-read
                     (concat "New tag: " (org-make-tag-string (org-get-tags nil t)))
                     (org-global-tags-completion-table)
                     nil
                     nil
                     nil
                     t))
      (when (not (equal new-tag ""))
        (org-toggle-tag new-tag 'on)))))

(defun zk-org-populate-todays-agenda-command ()
  "Find TODO entries scheduled for today that match a
 tag.  It's useful for populating meeting agenda."
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
       (when (zk-org-scheduled-for-today-p (org-element-at-point))
         (zk-org-generate-custom-id-at-point)
         (push (zk-org-get-external-reference) links)))
     (concat tag "/!")
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

(defun zk-org-setup-bindings ()
  "Register my own shortcuts for org mode"
  (local-set-key (kbd "C-c a") 'org-agenda-list)
  (local-set-key (kbd "C-c t") 'org-todo-list)
  (local-set-key (kbd "C-c m") 'zk-org-tags-view)
  (local-set-key (kbd "C-c s") 'zk-org-search-view)
  (local-set-key (kbd "C-c q") 'zk-org-set-tags-command)
  (local-set-key (kbd "C-c l i") 'zk-org-generate-custom-id-at-point)
  (local-set-key (kbd "C-c l a") 'zk-org-populate-todays-agenda-command)
  (local-set-key (kbd "C-c l l") 'zk-org-copy-external-link)
  (local-set-key (kbd "C-c l r") 'zk-org-copy-external-reference)
  (local-set-key (kbd "C-c l w") 'zk-org-copy-region-with-backlink)
  (local-set-key (kbd "C-c l b") 'zk-org-log-backlink-at-point)
  (local-set-key (kbd "C-c l f") 'zk-org-find-references-to-current-entry)
  (local-set-key (kbd "C-c l s") 'zk-org-fill-scratch-task-queue)
  (local-set-key (kbd "C-c r s") 'zk-zorg-show-status)
  (local-set-key (kbd "C-c r u") 'zk-zorg-rsync-upload)
  (local-set-key (kbd "C-c r d") 'zk-zorg-rsync-download)
  (local-set-key (kbd "C-c r C-d") 'zk-zorg-rsync-diff)
  (local-set-key (kbd "C-c r o") 'zk-zorg-set-outdated)
  (local-set-key (kbd "C-c e h") 'zk-org-export-html-to-clipboard)
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

(defun zk-zorg-frame-title-frame-name-function ()
  (let* ((file-name (buffer-name))
         (display-file-name
          (if (string-match-p ".*\\.org$" file-name)
              ;; For org files, remove the trailing ".org" when
              ;; included in frame titles
              (substring file-name 0 (- (length file-name) 4))
            file-name)))
    (or (and (not (buffer-base-buffer)) ; Keep the default title for
                                        ; indirect buffers
             (eq major-mode 'org-mode)
             (concat display-file-name
                     "/"
                     (zk-org-get-heading-string)))
        display-file-name)))

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

(when (display-graphic-p)
  (setq leuven-scale-outline-headlines nil
        leuven-scale-org-agenda-structure nil)
  (load-theme 'leuven t))

(provide 'zorg)
