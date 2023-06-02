(require 'zk)

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

(defun zk-org-setup-bindings ()
  "Register my own shortcuts for org mode"
  (local-set-key (kbd "C-c a") 'org-agenda-list)
  (local-set-key (kbd "C-c m") 'zk-org-tags-view)
  (local-set-key (kbd "C-c q") 'zk-org-set-tags-command)
  (local-set-key (kbd "C-c g n") 'zk-orgwork-goto-latest-note-file)
  (local-set-key (kbd "C-c g w") 'zk-orgwork-goto-orgwork-file)
  (local-set-key (kbd "C-c l") 'zk-org-set-generated-custom-id-and-copy-external-link))

(defun zk-org-set-file-encoding ()
  ;; Force unix newline format, even on Windows
  (setq buffer-file-coding-system 'utf-8-unix))

(add-hook 'org-mode-hook 'zk-org-setup-bindings)
(add-hook 'org-mode-hook 'zk-org-set-file-encoding)
(add-hook 'org-agenda-mode-hook 'zk-org-setup-bindings)

(provide 'zk-org)
