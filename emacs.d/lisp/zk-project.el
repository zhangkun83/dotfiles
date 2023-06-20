(require 'zk)

(defvar zk-project-root (expand-file-name command-line-default-directory)
  "The root directory of a project. TAGS and SRCFILES are located here.")

(defun zk-set-project-root(f)
  "Set project root where TAGS and SRCFILES are located."
  (interactive "DProject root: ")
  (setq zk-project-root f)
  (message "Project root set as %s" f)
  (setq zk-project-index-path (expand-file-name (concat zk-user-home-dir "/.zk/index/" zk-project-root))))

(zk-set-project-root zk-project-root)

(defun zk-bookmark-set ()
  "Set a bookmark with pre-populated name in zk's custom format."
  (interactive)
  (let ((file-name-at-point (buffer-file-name))
        (line-at-point
         (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (unless file-name-at-point
      (user-error "Current buffer doesn't have a file"))
    (bookmark-set
     (read-string
      "New bookmark: "
      (concat
       (zk-project-get-relative-path file-name-at-point)
       ": "
       (zk-trim-string line-at-point))))))

;; For jumping quickly back to project-root in shells.
;; Also used by gradlez script
(setenv "ZK_PROJECT_ROOT" zk-project-root)
(require 'savehist)
(require 'bookmark)
;; Save session data in per-zk-project directories
(let ((session-data-dir (expand-file-name (concat zk-user-home-dir "/.zk/emacs/" zk-project-root))))
  (make-directory session-data-dir t)
  (setq savehist-file (concat session-data-dir "history"))
  (savehist-mode t)
  (setq bookmark-default-file (concat session-data-dir "bookmarks"))
  ;; Do not display the file column in the benchmark menu, because zk-bookmark-set
  ;; already includes (nicer) file names in the bookmark names.
  (bookmark-bmenu-toggle-filenames nil)
  (global-set-key (kbd "C-x r m") 'zk-bookmark-set))

(provide 'zk-project)
