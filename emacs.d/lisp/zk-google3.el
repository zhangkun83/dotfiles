
(when (string-prefix-p "/google/src/cloud" command-line-default-directory)
  (require 'zk-project)
  (require 'zk)
  (require 'zk-clipboard)

  (defun zk-google3-find-g4-opened-file(f)
    "Find an opened file in the g4 client"
    (interactive
     (list (completing-read
            "Find an g4 opened file: "
            (-map 'zk-project-get-relative-path
                  (process-lines
                   "bash" "-c"
                   (concat "cd " zk-project-root "; g4 whatsout"))))))
    (find-file (zk-project-restore-absolute-path f)))
  (global-set-key (kbd "C-z g f") 'zk-google3-find-g4-opened-file)

  (defun zk-google3-open-in-codesearch()
    "Open the current file in codesearch and focus on the current line."
    (interactive)
    (browse-url (concat "http://cs/piper///depot/google3/"
            (zk-project-get-relative-path (buffer-file-name))
            ";l="
            (number-to-string (line-number-at-pos)))))
  (global-set-key (kbd "C-z g s") 'zk-google3-open-in-codesearch)

  (defun zk-google3-open-build-sponge-link()
    "Open the sponge link in the compilation buffer."
    (interactive)
    (let ((buffer (get-buffer "*compilation*")))
      (if buffer
          (with-current-buffer (get-buffer "*compilation*")
            (save-excursion
              (goto-char (point-min))
              (when (search-forward "Streaming build results to: http://sponge2")
                (zk-browse-url (thing-at-point 'url)))))
        (message "*compilation* buffer not found"))))
  (global-set-key (kbd "C-z g p") 'zk-google3-open-build-sponge-link)

  (defun zk-google3-open-critique()
    "Open the critique page of the current file. If the current
buffer doesn't visit a file, let the user select a CL to open."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (if file-name
          (shell-command (concat "open-critique-for-file "
                                 (prin1-to-string (zk-project-get-relative-path file-name))))
        ;; If not visiting a file, let the user select a CL
        (let* ((selected-line
                (completing-read "Open Critique for: "
                                 (process-lines "bash" "-c" "g4 p | grep '^[* ]*Change [0-9]\\+' | sed 's/^[* ]*Change //'")))
               (selected-cl (if (string-match "\\(^[0-9]+\\) .*" selected-line)
                                (match-string 1 selected-line)
                              (user-error "No CL selected"))))
          (zk-browse-url (concat "http://cl/" selected-cl))))))
  (global-set-key (kbd "C-z g c") 'zk-google3-open-critique)

  (defun zk-google3-open-file-from-codesearch-or-critique-link (link)
    "Open a file indicated by the given codesearch or critique link"
    (interactive (list
                  (read-string "CodeSearch or Critique link: " (zk-clipboard-get-string))))
    (cond ((string-prefix-p "https://critique.corp.google.com" link)
           (string-match "https://[a-z.]*/cl/[0-9]+/depot/google3/\\([^;?]+\\)\\(.*\\)" link))
          ((string-prefix-p "https://source.corp.google.com" link)
           (string-match "https://[a-z.]*/piper///depot/google3/\\([^;?]+\\)\\(.*\\)" link))
          (t (user-error "Not a CodeSearch or Critique link")))
    (let* ((path (match-string 1 link))
           (params-substring (match-string 2 link))
           (line-string (cond ((string-match ";l=\\([0-9]+\\)" params-substring) (match-string 1 params-substring))
                              ((string-match "#\\([0-9]+\\)" params-substring) (match-string 1 params-substring))
                              (t nil)))
           (line (if line-string (string-to-number line-string))))
      (if path
          (progn
            (switch-to-buffer (find-file-noselect (zk-project-restore-absolute-path path)))
            (if line (goto-line line)))
        (user-error "Cannot parse the link"))))
  (global-set-key (kbd "C-z g M-f") 'zk-google3-open-file-from-codesearch-or-critique-link)

  (defun zk-google3-g4-edit ()
    "g4-edit the given path."
    (interactive)
    (let ((output nil)
          (return-status nil)
          (buffer (current-buffer)))
      (with-temp-buffer
        (setq return-status (call-process "g4" nil t nil "edit" (file-name-nondirectory (buffer-file-name buffer))))
        (setq output (zk-trim-string (buffer-string))))
      (if (= 0 return-status)
          (revert-buffer))
        (message output)))
  (global-set-key (kbd "C-z g e") 'zk-google3-g4-edit)
)

(provide 'zk-google3)
