(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; Disable tool-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Disable scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable menu bar	
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Set default browser to chrome
(setq browse-url-browser-function 'zk-browse-url)

;;;; etags-select (better ctags search)

(load "etags-select.el")
(defun zk-load-tags-if-not-loaded ()
  (interactive)
  (unless (get-buffer "TAGS")
    (visit-tags-table zk-project-index-path)))

(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

;; From some point etags-select stopped loading TAGS.
;; Work around it.
(advice-add 'etags-select-find-tag :before #'zk-load-tags-if-not-loaded)

;;; Always do case-sensitive search for tags
(setq-default tags-case-fold-search nil)

;;;; Don't enable semantic (semantic doesn't work if Java file contains generics)
;;(semantic-mode 1)


;;; Ido
(require 'ido)
(ido-mode t)
;; Disable the annoying "searching in other directories" behavior of ido-find-file
(setq ido-auto-merge-work-directories-length -1)

(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))


;;; Load markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; Display line and column numbers on status bar
(setq column-number-mode t)


;; Disable tabs
(setq-default indent-tabs-mode nil)


;;; Turn on outline and showing matching parentheses for these languages.
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(add-hook 'java-mode-hook
  (lambda ()
    "Enable Java outline."
    (setq outline-regexp "\\(?:\\([ \t]*.*\\(class\\|interface\\)[ \t]+[a-zA-Z0-9_]+[ \t\n]*\\({\\|extends\\|implements\\)\\)\\|[ \t]*.*\\(public\\|private\\|static\\|final\\|native\\|synchronized\\|transient\\|volatile\\|strictfp\\| \\|\t\\)*[ \t]+\\(\\([a-zA-Z0-9_]\\|\\( *\t*< *\t*\\)\\|\\( *\t*> *\t*\\)\\|\\( *\t*, *\t*\\)\\|\\( *\t*\\[ *\t*\\)\\|\\(]\\)\\)+\\)[ \t]+[a-zA-Z0-9_]+[ \t]*(\\(.*\\))[ \t]*\\(throws[ \t]+\\([a-zA-Z0-9_, \t\n]*\\)\\)?[ \t\n]*{\\)")))
(add-hook 'java-mode-hook 'outline-minor-mode)
(add-hook 'java-mode-hook 'show-paren-mode)
(add-hook 'java-mode-hook
          (lambda ()
            "A few code-style parameters for Java"
            (set-fill-column 100)
            ;; Do case-sensitive search within Java code
            (setq case-fold-search nil)
            (setq c-basic-offset 2
                  tab-width 2)
            ;; For newlines in argument list, replace the default indentation that aligns with
            ;; the parentheses, with the Google style that use double indentations (++)
            (c-set-offset 'arglist-cont-nonempty '++)
            (c-set-offset 'arglist-intro '++)
            ;; This was single indentation, should be double.
            (c-set-offset 'statement-cont '++)
            (c-set-offset 'annotation-var-cont 0)
            ;; case: line wasn't indenting. It should be.
            (c-set-offset 'case-label '+)
            ))

;;; Tramp related
(add-hook 'before-save-hook `zk-save-remote-file-as-local-copy)
(setq tramp-default-method "ssh")

;;; Quickly switch between the startup directory and current file's
(defun zk-cd-initial()
  "Change to the initial directory from which emacs was started"
  (interactive)
  (cd command-line-default-directory)
  (message default-directory))

(defun zk-cd-current-buffer()
  "Change to the directory of the current file"
  (interactive)
  (if buffer-file-name
      (progn
        (cd (file-name-directory buffer-file-name))
        (message default-directory))
    (message "Current buffer does not have a file"))
)

;; For jumping quickly back to project-root in shells.
;; Also used by gradlez script
(require 'zk)
(setenv "ZK_PROJECT_ROOT" zk-project-root)
(setq frame-title-format '("" command-line-default-directory " - emacs"))

;; Bookmarks-related
;; Use per-zk-project bookmark file
(let ((bookmark-dir (expand-file-name (concat "~/.zk/emacs-bookmarks/" zk-project-root))))
  (make-directory bookmark-dir t)
  (setq bookmark-default-file (concat bookmark-dir "/bookmarks")))
;; Do not display the file column in the benchmark menu, because zk-bookmark-set
;; already includes (nicer) file names in the bookmark names.
(require 'bookmark)
(bookmark-bmenu-toggle-filenames nil)
(global-set-key (kbd "C-x r m") 'zk-bookmark-set)

;; Don't create backup files at all
(setq make-backup-files nil)

(setenv "EDITOR" "~/.emacs.d/bin/editor-stub")
(setenv "P4EDITOR" "~/.emacs.d/bin/editor-stub")
(setenv "P4DIFF" "diff -u")  ; works with zk-diff-navigate
(setenv "G4PENDINGSTYLE" "relativepath")

(add-hook 'java-mode-hook
	  (lambda()
	    "Register my own shortcuts for Java mode"
	    (local-set-key (kbd "C-c i") 'zk-insert-java-import)
	    (local-set-key (kbd "C-c d") 'zk-c-toggle-syntactic-indentation)
            (local-set-key (kbd "M-;") 'recenter-top-bottom)
            (local-set-key (kbd "M-h") 'zk-java-mark-thing)
            (local-set-key (kbd "M-i") 'zk-java-enter-braces-block)
            (local-set-key (kbd "M-o") 'zk-java-exit-bracesblock)
            (local-set-key (kbd "M-n") 'zk-java-next-thing)
            (local-set-key (kbd "M-p") 'zk-java-prev-thing)
            (local-set-key [backtab] (lambda() (interactive) (c-indent-line-or-region -1)))))

(global-set-key (kbd "C-x M-f") 'zk-find-src-file-in-project)

; Java stacktrace detection in compilation-mode
(require 'zk-java-stacktrace)
(zk-java-stacktrace-detection-enable)
(global-set-key (kbd "C-x \\") 'compile)
(global-set-key (kbd "C-x |") 'compilation-minor-mode)

(setq compile-command "runp ")

;; go-mode for Golang
(autoload 'go-mode "go-mode" nil t)
(autoload 'gofmt-before-save "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook
          (lambda()
            (setq tab-width 2)))

(add-to-list 'compilation-search-path zk-project-root)
(require 'grep)
(add-to-list 'grep-search-path zk-project-root)

(defun zk-prev-window()
  "Switch to previous window"
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-_") 'zk-prev-window)
(global-set-key (kbd "M-+") 'other-window)
(global-set-key (kbd "M-*") 'switch-to-buffer)
(global-set-key (kbd "M-~") 'revert-buffer)
(global-set-key (kbd "C-'") 'switch-to-buffer)
(global-set-key (kbd "C-c p") 'zk-copy-buffer-file-path)
(global-set-key (kbd "C-c b") 'browse-url)
(define-key minibuffer-local-map (kbd "C-c p") 'zk-minibuffer-insert-current-file-path)
(define-key minibuffer-local-map (kbd "C-c n") 'zk-minibuffer-insert-current-file-name)
(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "C-c M-w") 'zk-clipboard-save)
(global-set-key (kbd "C-c C-w") 'zk-clipboard-kill)
(global-set-key (kbd "C-c C-y") 'zk-clipboard-yank)
(global-set-key (kbd "C-M-l") 'zk-recenter-top-bottom-other-window)
(global-set-key (kbd "C-x /") 'zk-diff-navigate)

(add-hook 'shell-mode-hook
          (lambda()
            "Make dots part of the word so full paths can be expanded by M+/"
            (make-local-variable 'dabbrev-abbrev-char-regexp)
            (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_\\|\\.")
            (local-set-key (kbd "C-c o") 'zk-open-file-path-from-region-or-at-point)
            (local-set-key (kbd "C-c c") 'zk-shell-command-on-file-at-point)
            (local-set-key (kbd "C-c t") 'comint-truncate-buffer)))

(add-hook 'org-mode-hook
	  (lambda()
	    "Register my own shortcuts for org mode"
	    (local-set-key (kbd "C-c a") 'org-agenda)))

;; Set font
(set-face-attribute 'default nil
		    :family "Liberation Mono")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-echo-syntactic-information-p t)
 '(compilation-scroll-output t)
 '(custom-enabled-themes (if (display-graphic-p) (quote (leuven))))
 '(dabbrev-case-replace nil)
 '(explicit-bash-args
   (quote
    ("--noediting" "--rcfile" "~/.emacs.d/zk-bash-init.sh" "-i")))
 '(fci-rule-character 9474)
 '(global-hl-line-sticky-flag t)
 '(ido-enable-flex-matching nil)
 '(inhibit-startup-screen t)
 '(ns-command-modifier nil)
 '(org-startup-indented t)
 '(tags-revert-without-query t)
 '(use-dialog-box nil))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'zk-go-to-char)
(global-set-key (kbd "C-f") 'zk-go-to-char-forward)
(global-set-key (kbd "C-b") 'zk-go-to-char-backward)

;; Treat camelCase as multiple words instead of one, and treat
;; punctuation marks as words as well.
(require 'syntax-subword)
(global-syntax-subword-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "blue" :foreground "white")))))
