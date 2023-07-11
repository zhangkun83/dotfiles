(require 'zorg)
(setq zk-zorg-rsync-backup-dir
      "rsync://localhost:1873/x20/users/zh/zhangkun/orgwork")
(setq zk-zorg-profile-name "orgwork")
(setq zk-zorg-startup-view-func
      (lambda ()
        (org-tags-view nil "keep_in_mind")
        (split-window)
        (org-tags-view nil "tbs")))

(zk-zorg-startup-init)
