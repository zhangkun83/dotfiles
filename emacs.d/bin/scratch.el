;; Set up the scratch environment

(require 'zk)
(setq zk-frame-title-base-name "scratch")
(setq confirm-kill-emacs 'yes-or-no-p)
(if (display-graphic-p)
    (load-theme 'deeper-blue t))

(defconst zk-scratch-dir (concat zk-user-home-dir "/scratch"))

(defun zk-scratch-get-link-at-point()
  (let ((link-prop (get-text-property (point) 'htmlize-link)))
    (when link-prop
      (nth 1 link-prop))))

(defun zk-scratch-org-up-element()
  (ignore-errors
    (org-up-element)))

(defun zk-scratch-org-backward-heading-same-level()
  (org-backward-heading-same-level nil))

(defun zk-scratch-insert-to-task-queue (content)
  "Add a bullet line to the beginning of the task queue, which is
the first top-level heading of the scratch file."
  (zk-scratch-open-org-file)
  (zk-repeat-until-stuck 'zk-scratch-org-up-element)
  (org-forward-heading-same-level nil)
  (zk-repeat-until-stuck 'zk-scratch-org-backward-heading-same-level)
  (move-end-of-line nil)
  (newline)
  (insert "- " content))

(require 'server)
(setq server-name "scratch")
(server-start)

(defun zk-scratch-advice-open-link-at-point(orig-open-link-at-point)
  "Opens the link at point. If it's a local org link, ask the zorg
server to open it.  The link format must be like
'file:@orglife:notes2023.org::#node_id'"
  (interactive)
  (let ((link (zk-scratch-get-link-at-point)))
    (when link
      (if (string-match-p "^file:.*" link)
          (progn
            (unless (string-match "^file:@\\([a-z]+\\):\\(.*\\)$" link)
              (user-error "Link format must be in the form 'file:@zorgprofile:filename.org::#nodeid'"))
            (let ((zorg-profile (match-string-no-properties 1 link))
                  (actual-link (concat "file:" (match-string-no-properties 2 link))))
              ;; Open org file links in zorg
              (server-eval-at
               zorg-profile
               (list 'progn
                     (list 'org-link-open-from-string actual-link)
                     '(raise-frame)))
              (message "Asked %s to open \"%s\"" zorg-profile actual-link)))
        ;; Open other links normally
        (funcall orig-open-link-at-point)))))

(advice-add 'org-open-at-point :around #'zk-scratch-advice-open-link-at-point)

(global-set-key (kbd "<f5>") 'zk-scratch-init)
(global-set-key (kbd "<f6>") 'zk-scratch-open-lisp-window)

(setq create-lockfiles nil)
(kill-buffer "*scratch*")

(defun zk-scratch-open-org-file()
  (find-file (concat zk-scratch-dir "/scratch-org.org")))

(defun zk-scratch-init ()
  (interactive)
  (delete-other-windows)
  (zk-scratch-open-org-file))

(defun zk-scratch-open-lisp-window ()
  (interactive)
  (find-file-other-window (concat zk-scratch-dir "/scratch-lisp"))
  (lisp-interaction-mode))

(zk-scratch-init)
