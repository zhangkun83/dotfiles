;; Set up the scratch environment

(setq frame-title-format "scratch")
(setq confirm-kill-emacs 'yes-or-no-p)
(disable-theme 'leuven)
(load-theme 'adwaita t nil)

(defconst zk-scratch-dir (concat zk-user-home-dir "/scratch"))

(defun zk-scratch-get-link-at-point()
  (let ((link-prop (get-text-property (point) 'htmlize-link)))
    (when link-prop
      (nth 1 link-prop))))

(require 'server)
(defun zk-scratch-open-link-at-point()
  "Opens the link at point. If it's a local org link, ask the
orgwork server to open it."
  (interactive)
  (let ((link (zk-scratch-get-link-at-point)))
    (when link
      (if (string-match-p "^file:.*" link)
          ;; Open org file links in orgwork
          (progn
            (server-eval-at
             "orgwork"
             (list 'progn
                     (list 'org-link-open-from-string link)
                     '(raise-frame)))
            (message "Asked orgwork to open \"%s\"" link))
        ;; Open other links normally
        (org-open-at-point)))))

(advice-add 'org-open-at-point :override #'zk-scratch-open-link-at-point)

(global-set-key (kbd "<f5>") 'zk-scratch-init)

(setq create-lockfiles nil)
(auto-save-visited-mode)
(kill-buffer "*scratch*")

(defun zk-scratch-init ()
  (interactive)
  (delete-other-windows)
  (find-file (concat zk-scratch-dir "/scratch-org.org"))
  (split-window-below)
  (other-window 1)
  (find-file (concat zk-scratch-dir "/scratch-lisp"))
  (lisp-interaction-mode)
  (other-window 1))

(zk-scratch-init)
