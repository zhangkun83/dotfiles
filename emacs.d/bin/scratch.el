;; Set up the scratch environment
(require 'zk-org)

(when (display-graphic-p)
  (load-theme 'modus-operandi-tinted t)
  (zk-org-init-fonts))

(require 'zk)
(zk-start-server-or-create-frame "scratch")


(setq zk-frame-title-base-name "scratch")
(setq confirm-kill-emacs 'yes-or-no-p)

(defconst zk-scratch-dir (concat zk-user-home-dir "/scratch"))
(defconst zk-kanban-file (concat zk-scratch-dir "/kanban.org"))

(defun zk-scratch-get-link-at-point()
  (let ((link-prop (get-text-property (point) 'htmlize-link)))
    (when link-prop
      (nth 1 link-prop))))

(defun zk-scratch-org-up-element()
  (ignore-errors
    (org-up-element)))

(defun zk-scratch-org-backward-heading-same-level ()
  (org-backward-heading-same-level nil))

(defun zk-scratch-remote-locate-in-task-queue (id)
  "(To be called from a client) locate the ID in the task queues.
Raises the scratch frame if found.  Returns a message indicating
the result."
  ;; If error occurs on the server side, the call will be stuck.  It
  ;; seems the error cannot be sent back to the client.  So we catch
  ;; the errors and return the message.
  (condition-case err
      (let ((pos (point)))
        (zk-scratch-open-kanban-file)
        (save-excursion
          (goto-char 1)
          (search-forward (concat "[" id "]"))
          (org-beginning-of-line)
          (setq pos (point))
          (raise-frame))
        (goto-char pos)
        (message "scratch: located '%s'" id))
    (error (message "scratch: %s" (error-message-string err)))))

(defun zk-scratch-remote-insert-all-to-task-queue (ref-metadata-list)
  "(To be called from a client) For each reference metadata in
ref-metadata-list, in the form of ((category1 content1
id1) (category2 content2 id2) ...), add a bullet line to the
beginning of the category heading, if the id doesn't appear in
the queue.  Raises the scratch frame.  Returns a message
indicating the result."
  ;; If error occurs on the server side, the call will be stuck.  It
  ;; seems the error cannot be sent back to the client.  So we catch
  ;; the errors and return the message.
  (condition-case err
      (let ((inserted-count 0))
        (dolist (elt ref-metadata-list inserted-count)
          (when (zk-scratch-insert-to-task-queue
                 (nth 0 elt)
                 (nth 1 elt)
                 (nth 2 elt))
            (setq inserted-count (+ inserted-count 1))))
        (raise-frame)
        (message "scratch: added %d of %d tasks" inserted-count (length ref-metadata-list)))
    (error (message "scratch: error in zk-scratch-remote-insert-all-to-task-queue: %s"
                   (error-message-string err)))))

(defun zk-scratch-insert-to-task-queue (category content id)
  "Add a bullet line to the beginning of the heading whose name
is category, if the id doesn't appear in the queue.  Returns t if
inserted, nil if already exists."
  (zk-scratch-open-kanban-file)
  (let ((case-fold-search nil))
    (goto-char (point-min))
    ;; If the category heading doesn't exist, create it
    (unless (search-forward-regexp (concat "^\\* " (regexp-quote category)) nil t)
      (insert (concat "* " category))
      (open-line 1))
    (save-restriction
      (org-narrow-to-subtree)
      (if (save-excursion
            (search-forward (concat "[" id "]") nil t))
          nil
        (newline)
        (insert "- % " content)
        t))))

(defun zk-scratch-advice-open-link-from-string (orig-open-link-from-string
                                                link &optional arg)
  "Opens the given link. If it's a local org link, ask the zorg server to
open it.  The link format must be like
'file:@orglife:notes2023.org::#node_id'"
  (if (string-match-p "^file:.*" link)
      (progn
        (unless (string-match "^file:@\\([a-z]+\\):\\(.*\\)$" link)
          (user-error "Link format must be in the form 'file:@zorgprofile:filename.org::#nodeid'"))
        (let ((zorg-profile (match-string-no-properties 1 link))
              (actual-link (concat "file:" (match-string-no-properties 2 link))))
          ;; Open org file links in zorg
          (if (server-eval-at
               zorg-profile
               `(ignore-errors
                  ;; org-link-open-from-string throws an error if the
                  ;; link cannot be opened.  That will be turned into
                  ;; a nil by ignore-errors.
                  (zk-zorg-link-open-from-string ,actual-link)
                  (raise-frame)
                  t))
              (message "Asked %s to open link \"%s\"" zorg-profile actual-link)
            (message "%s failed to open link \"%s\"" zorg-profile actual-link))))
    ;; Open other links normally
    (funcall orig-open-link-from-string link arg)))

(advice-add 'org-open-link-from-string :around #'zk-scratch-advice-open-link-from-string)

(global-set-key (kbd "<f5>") 'zk-scratch-init)
(global-set-key (kbd "<f6>") 'zk-scratch-open-lisp-window)


;; Highlight the #orglife and #orgwork tags
(defface zk-hl-orglife
  `((t :background "lavender" :height 0.75 :family ,zk-font-family))
  "A custom face for tagging #orglife")

(defface zk-hl-orgwork
  `((t :height 0.75 :family ,zk-font-family))
  "A custom face for tagging #orgwork")

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-o") 'zk-org-open-next-link)
            (local-set-key (kbd "C-c e h") 'zk-org-export-html-to-clipboard)
            (when (equal buffer-file-name zk-kanban-file)
              (font-lock-add-keywords
               nil
               '(("#orglife\\b" . 'zk-hl-orglife)
                 ("#orgwork\\b" . 'zk-hl-orgwork))))))

(setq create-lockfiles nil)
(kill-buffer "*scratch*")

(defun zk-scratch-open-kanban-file()
  (find-file zk-kanban-file))

(defun zk-scratch-init ()
  (interactive)
  (delete-other-windows)
  (zk-scratch-open-kanban-file))

(defun zk-scratch-open-lisp-window ()
  (interactive)
  (find-file-other-window (concat zk-scratch-dir "/scratch-lisp"))
  (lisp-interaction-mode))

(zk-scratch-init)
