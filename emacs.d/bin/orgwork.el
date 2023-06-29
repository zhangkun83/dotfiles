(require 'zk)

(defconst zk-orgwork-rsync-backup-dir
      "rsync://localhost:1873/x20/users/zh/zhangkun/orgwork")

;; Use relative directory for local paths, because I may be running
;; this script under MINGW64 (provided by Git bash) whose home is like
;; "/c/Users/zhangkun" while the rsync on the system is a Cygwin
;; version whose home is like "/cygdrive/c/home/zhangkun". Passing the
;; absolute path from this script to rsync won't work.
(defconst zk-orgwork-dirname "orgwork")

(defconst zk-orgwork-rsync-buffer-name "*orgwork rsync*")

(defconst zk-orgwork-directory (concat zk-user-home-dir "/" zk-orgwork-dirname))

(defconst zk-orgwork-upload-list-file-name ".upload-list")

;; Possible values: outdated, downloading, uploading, clean, modified
(setq zk-orgwork-status 'outdated)

;; Most values are copied from the default mode-line-format. I added
;; zk-orgwork-status.
(setq-default
 mode-line-format
 '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
   "  " mode-line-modes
   (:eval (format "[orgwork: %s]" zk-orgwork-status))
   mode-line-misc-info mode-line-end-spaces))

(defun zk-orgwork-generate-upload-list-file ()
  "Generates a file that has the list of files eligible for
uploading. It only include org and org_archive files, and exclude
Emacs temporary files (starting with #) and hidden
files (starting with .)"
  (let ((list-file (concat zk-orgwork-directory "/" zk-orgwork-upload-list-file-name))
        (file-list (directory-files zk-orgwork-directory)))
    (with-current-buffer (find-file-noselect list-file)
      (erase-buffer)
      (dolist (file file-list)
        (when (string-match-p "^[^.#].+\\.\\(org\\)\\|\\(org_archive\\)$" file)
          (insert file "\n")))
      (save-buffer)
      (kill-buffer))))

(defun zk-orgwork-goto-latest-note-file ()
  "Go to the latest note org file under the same directory."
  (interactive)
  (let* ((file-list (directory-files zk-orgwork-directory nil "notes.*\\.org"))
         ;; directory-files sort the files alphabeticaly
         (latest-file (car (last file-list))))
    (unless latest-file (user-error "No notes file found"))
    (switch-to-buffer (find-file-noselect (concat zk-orgwork-directory "/" latest-file)))))

(defun zk-orgwork-goto-orgwork-file ()
  "Go to the work.org file under the same directory if it exists."
  (interactive)
  (let ((file-name "work.org"))
    (unless (file-exists-p file-name) (user-error "%s does not exist" file-name))
    (switch-to-buffer (find-file-noselect file-name))))

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
  (require 'org)
  (let ((found-p nil))
    (org-map-entries (lambda ()
                       (if (string= custom-id
                               (org-element-property :CUSTOM_ID (org-element-at-point)))
                           (setq found-p t))))
    found-p))
  
(defun zk-org-set-generated-custom-id-and-copy-external-link ()
  "If CUSTOM_ID of the current org headline doesn't exist,
generate one based on the text of the headline and set it. This
will also move to that headline. Return the external link based
on the CUSTOM_ID."
  (interactive)
  (require 'org)
  (require 'org-element)
  (zk-org-move-to-current-heading)
  (let* ((headline (org-element-at-point))
         (custom-id (or
                     (org-element-property :CUSTOM_ID headline)
                     (let ((new-id
                            (zk-org-generate-custom-id-from-text
                             (substring-no-properties (org-get-heading t t t t)))))
                       (org-set-property "CUSTOM_ID" new-id)
                       new-id)))
         (link (concat "file:"
                       (file-name-nondirectory (buffer-file-name (current-buffer)))
                       "::#"
                       custom-id)))
    (kill-new link)
    (message "Copied \"%s\"" link)))

(defun zk-org-move-to-current-heading ()
  "Move to the current heading if not already at a heading."
  (interactive)
  (require 'org-element)
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

(require 'dash)
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

(defun zk-orgwork-show-status ()
  (interactive)
  (message "orgwork status: %s" zk-orgwork-status))

(defun zk-org-setup-bindings ()
  "Register my own shortcuts for org mode"
  (local-set-key (kbd "C-c a") 'org-agenda-list)
  (local-set-key (kbd "C-c m") 'zk-org-tags-view)
  (local-set-key (kbd "C-c q") 'zk-org-set-tags-command)
  (local-set-key (kbd "C-c g n") 'zk-orgwork-goto-latest-note-file)
  (local-set-key (kbd "C-c g w") 'zk-orgwork-goto-orgwork-file)
  (local-set-key (kbd "C-c l") 'zk-org-set-generated-custom-id-and-copy-external-link)
  (local-set-key (kbd "C-c s s") 'zk-orgwork-show-status)
  (local-set-key (kbd "C-c s u") 'zk-orgwork-rsync-upload))

(defun zk-org-set-file-encoding ()
  ;; Force unix newline format, even on Windows
  (setq buffer-file-coding-system 'utf-8-unix))

(defun zk-orgwork-rsync-download ()
  (unless (eq zk-orgwork-status 'outdated)
    (user-error "Unexpected orgwork status: %s" zk-orgwork-status))
  (setq zk-orgwork-status 'downloading)
  (let ((default-directory zk-orgwork-directory))
    (switch-to-buffer zk-orgwork-rsync-buffer-name)
    (erase-buffer)
    (insert "Downloading remote files ...\n")
    (make-process :name "orgwork-rsync-download"
                  :buffer zk-orgwork-rsync-buffer-name
                  :command (list
                            "rsync" "-rtuv"
                            (concat zk-orgwork-rsync-backup-dir "/") ".")
                  :sentinel (lambda (process event)
                              (message "orgwork-rsync-download is now %s" event)
                              (if (string-match-p "finished.*" event)
                                  (progn
                                    (setq zk-orgwork-status 'clean)
                                    (read-string "Download successful. Press Enter to continue ...")
                                    (kill-buffer)
                                    (zk-orgwork-startup-open nil))
                                (setq zk-orgwork-status 'outdated)
                                (if (y-or-n-p "Download failed. Press y to retry, n to open in read-only mode")
                                    (zk-orgwork-rsync-download)
                                  (kill-buffer)
                                  (zk-orgwork-startup-open t)))))))


(defun zk-orgwork-rsync-upload ()
  (interactive)
  (unless (or
           (eq zk-orgwork-status 'clean)
           (eq zk-orgwork-status 'modified))
    (user-error "Unexpected orgwork status: %s" zk-orgwork-status))
  (setq zk-orgwork-status 'uploading)
  (zk-orgwork-generate-upload-list-file)
  (let ((default-directory zk-orgwork-directory))
    (switch-to-buffer zk-orgwork-rsync-buffer-name)
    (erase-buffer)
    (insert "Uploading local changes ...\n")
    (make-process :name "orgwork-rsync-upload"
                  :buffer zk-orgwork-rsync-buffer-name
                  :command (list
                            "rsync" "-rtuv"
                            (concat "--files-from=" zk-orgwork-upload-list-file-name)
                            "./" zk-orgwork-rsync-backup-dir)
                  :sentinel (lambda (process event)
                              (message "orgwork-rsync-upload is now %s" event)
                              (if (string-match-p "finished.*" event)
                                  (progn
                                    (setq zk-orgwork-status 'clean)
                                    (read-string "Upload successful. Press Enter to continue ...")
                                    (kill-buffer))
                                (setq zk-orgwork-status 'modified)
                                (if (y-or-n-p "Upload failed. Retry?")
                                    (zk-orgwork-rsync-upload)))))))

(defun zk-orgwork-startup-open (readonly)
  (when readonly
      (add-hook 'org-mode-hook (lambda() (read-only-mode 1))))
  (org-tags-view nil "keep_in_mind")
  (split-window)
  (org-tags-view nil "tbs")
  (setq server-name "orgwork")
  (server-start)
  (message "Ready%s. Have a very safe and productive day!"
           (if readonly " (read-only)" "")))

(defun zk-orgwork-shutdown-confirm (prompt)
  (if (eq zk-orgwork-status 'modified)
      (string-equal
       (read-string "Some modifications have not been uploaded. Type \"I want to quit!\" if you really want to quit: ")
       "I want to quit!")
    (yes-or-no-p prompt)))

(add-hook 'before-save-hook
          (lambda ()
            (if (eq zk-orgwork-status 'clean)
                (setq zk-orgwork-status 'modified))))

(add-hook 'org-mode-hook 'zk-org-setup-bindings)
(add-hook 'org-mode-hook 'zk-org-set-file-encoding)
(add-hook 'org-agenda-mode-hook 'zk-org-setup-bindings)

(setq org-agenda-files (list zk-orgwork-directory)
      frame-title-format "orgwork" )

(setq confirm-kill-emacs 'zk-orgwork-shutdown-confirm)

(zk-orgwork-rsync-download)
