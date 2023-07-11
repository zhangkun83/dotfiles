(require 'zorg)
(setq zk-zorg-rsync-backup-dir
      "rsync://localhost:1874/backup/orglife")
(setq zk-zorg-profile-name "orglife")
(setq zk-zorg-startup-view-func
      (lambda ()
        (org-agenda-list)))
(zk-zorg-startup-init)
