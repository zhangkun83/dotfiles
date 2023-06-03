(require 'zk-org)

(defconst zk-orgwork-rsync-backup-dir
      "rsync://localhost:1873/x20/users/zh/zhangkun/orgwork")

(defconst zk-orgwork-dirname "orgwork")
(defconst zk-orgwork-rsync-buffer-name "*orgwork rsync*")

;; Possible values: outdated, downloading, uploading, clean, modified
(setq zk-orgwork-status 'outdated)

(defconst zk-orgwork-rsync-invoke-dir
  (if (eq system-type 'windows-nt)
      ;; On Windows $USERPFOFILE is
      ;; C:\Users\{username}, while $HOME is
      ;; c:\Users\{username}\AppData\Roaming
      (getenv "USERPROFILE")
    (getenv "HOME")))
  
(defun zk-orgwork-rsync-download ()
  (unless (eq zk-orgwork-status 'outdated)
    (user-error "Unexpected orgwork status: %s" zk-orgwork-status))
  (setq zk-orgwork-status 'downloading)
  (let ((default-directory zk-orgwork-rsync-invoke-dir))
    (switch-to-buffer zk-orgwork-rsync-buffer-name)
    (erase-buffer)
    (insert "Downloading remote files ...\n")
    (make-process :name "orgwork-rsync-download"
                  :buffer zk-orgwork-rsync-buffer-name
                  :command (list
                            "rsync" "-rtuv"
                            (concat zk-orgwork-rsync-backup-dir "/") zk-orgwork-dirname)
                  :sentinel (lambda (process event)
                              (message "orgwork-rsync-download is now %s" event)
                              (if (string-match-p "finished.*" event)
                                  (progn
                                    (setq zk-orgwork-status 'clean)
                                    (read-string "Download successful. Press Enter to open orgwork.")
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
  (let ((default-directory zk-orgwork-rsync-invoke-dir))
    (switch-to-buffer zk-orgwork-rsync-buffer-name)
    (erase-buffer)
    (insert "Uploading local changes ...\n")
    (make-process :name "orgwork-rsync-upload"
                  :buffer zk-orgwork-rsync-buffer-name
                  :command (list
                            "rsync" "-rtuv" "--include='*.org'" "--include='*.org_archive'" "--exclude='*'"
                            (concat zk-orgwork-dirname "/") zk-orgwork-rsync-backup-dir)
                  :sentinel (lambda (process event)
                              (message "orgwork-rsync-upload is now %s" event)
                              (if (string-match-p "finished.*" event)
                                  (progn
                                    (setq zk-orgwork-status 'clean)
                                    (read-string "Upload successful. Press Enter to continue.")
                                    (kill-buffer))
                                (setq zk-orgwork-status 'modified)
                                (if (y-or-n-p "Upload failed. Retry?")
                                    (zk-orgwork-rsync-upload)))))))

(defun zk-orgwork-startup-open (readonly)
  (when readonly
      (add-hook 'org-mode-hook (lambda() (read-only-mode 1))))
  (zk-orgwork-goto-latest-note-file)
  (end-of-buffer)
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

(setq zk-orgwork-directory (concat zk-orgwork-rsync-invoke-dir "/" zk-orgwork-dirname))
(setq org-agenda-files (list zk-orgwork-directory)
      frame-title-format "orgwork" )

(setq confirm-kill-emacs 'zk-orgwork-shutdown-confirm)

(zk-orgwork-rsync-download)
