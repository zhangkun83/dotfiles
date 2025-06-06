(require 'zorg)
(setq zk-zorg-rsync-backup-dir
      "rsync://127.0.0.1:1873/x20/users/zh/zhangkun/orgwork")
(setq zk-zorg-profile-name "orgwork")
(setq zk-zorg-startup-view-func
      (lambda ()
        (org-agenda-list)
        (split-window)
        (zk-zorg-open-tbs-agenda)))

(zk-zorg-startup-init)
