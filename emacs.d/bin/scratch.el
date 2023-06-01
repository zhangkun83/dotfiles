;; Set up the scratch environment

(setq frame-title-format "scratch")
(setq confirm-kill-emacs 'yes-or-no-p)

(defun zk-scratch-get-link-at-point()
  (let ((link-prop (get-text-property (point) 'htmlize-link)))
    (when link-prop
      (nth 1 link-prop))))

(require 'server)
(defun zk-scratch-open-link-at-point()
  (interactive)
  (let ((link (zk-scratch-get-link-at-point)))
    (when link
      (if (string-match-p "^file:.*" link)
          ;; Open org file links in orgwork
          (progn
            (server-eval-at
             "orgwork"
             (list 'org-link-open-from-string link))
            (message "Told orgwork to open \"%s\"" link))
        ;; Open other links normally
        (org-open-at-point)))))

(add-hook 'org-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c C-o") 'zk-scratch-open-link-at-point)))

(setq create-lockfiles nil)
(auto-save-visited-mode)
(find-file "~/.zk/scratch.org")
(split-window-below)
(other-window 1)
(find-file "~/.zk/scratch.el")
(lisp-interaction-mode)
(other-window 1)
(kill-buffer "*scratch*")
