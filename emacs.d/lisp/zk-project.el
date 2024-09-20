(require 'zk)

(defconst zk-project-root
  (expand-file-name command-line-default-directory)
  "The root directory of a project. TAGS and SRCFILES are located here.")

(defconst zk-project-root-as-suffix
  (replace-regexp-in-string ":" "" zk-project-root))

(defconst zk-project-index-path
  (expand-file-name (concat zk-user-home-dir "/.zk/index/" zk-project-root-as-suffix)))

(defun zk-project-get-relative-path(absolute-path)
  "If the absolute path starts with zk-project-root, remove it and make it a relative path"
  (if (string-prefix-p zk-project-root absolute-path)
      (let ((trimmed (substring absolute-path (length zk-project-root))))
        (if (string-prefix-p "/" trimmed)
            (substring trimmed 1)
          trimmed))
    absolute-path))

(defun zk-project-restore-absolute-path(relative-path)
  "If the path is a relative path, add zk-project-root as its prefix"
  (if (string-prefix-p "/" relative-path)
      relative-path
    (concat zk-project-root "/" relative-path)))

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
(let ((session-data-dir
       (expand-file-name (concat zk-user-home-dir "/.zk/emacs/" zk-project-root-as-suffix))))
  (make-directory session-data-dir t)
  (setq savehist-file (concat session-data-dir "history"))
  (savehist-mode t)
  (setq bookmark-default-file (concat session-data-dir "bookmarks"))
  ;; Do not display the file column in the benchmark menu, because zk-bookmark-set
  ;; already includes (nicer) file names in the bookmark names.
  (bookmark-bmenu-toggle-filenames nil)
  ;; The name column's default length is 30, which is too short
  (setq bookmark-bmenu-file-column 75)
  (global-set-key (kbd "C-x r m") 'zk-bookmark-set))

(provide 'zk-project)
