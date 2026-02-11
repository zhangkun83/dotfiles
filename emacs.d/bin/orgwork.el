(require 'zorg)
(setq zk-zorg-rsync-backup-dir
      "rsync://127.0.0.1:1873/repo/orgwork")
(setq zk-zorg-profile-name "orgwork")
(setq zk-zorg-meeting-notes-level 2)
(setq zk-zorg-startup-view-func
      (lambda ()
        (org-agenda-list)
        (zk-zorg-open-tbs-agenda)))

(zk-zorg-startup-init)
