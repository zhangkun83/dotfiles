(require 'zorg)
(setq zk-zorg-rsync-backup-dir
      "rsync://127.0.0.1:1874/backup/orglife")
(setq zk-zorg-profile-name "orglife")
(setq zk-zorg-meeting-notes-level 1)
(setq zk-zorg-startup-view-func
      (lambda ()
        (org-agenda-list)))
(zk-zorg-startup-init)
