(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/groovy-modes"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/org/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))

;; Load site-specific bits
(load "init-site.el")

;; Disable tool-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Disable scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable menu bar	
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Set default browser to chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;;; Org-mode
(load-library "find-lisp")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-agenda-files (find-lisp-find-files "~/org" "\.org$"))


;; Slime
(require 'slime)
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

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


;;;; Load lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;;; Load groovy-mode
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))


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
(require 'fill-column-indicator)
(add-hook 'java-mode-hook
          (lambda ()
            "A few code-style parameters for Java"
            (set-fill-column 100)
            (fci-mode)
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
            ;; Treat camelCase as multiple words instead of one
            (subword-mode)
            ))


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

(setenv "EDITOR" "editor-stub")
(setenv "P4EDITOR" "editor-stub")

(add-hook 'java-mode-hook
	  (lambda()
	    "Register my own shortcuts for Java mode"
	    (local-set-key (kbd "C-c i") 'zk-insert-java-import)
            (local-set-key (kbd "M-;") 'recenter-top-bottom)
            (local-set-key (kbd "M-h") 'zk-java-mark-thing)
            (local-set-key (kbd "M-i") 'zk-java-enter-braces-block)
            (local-set-key (kbd "M-o") 'backward-up-list)
            (local-set-key (kbd "M-n") 'zk-java-next-thing)
            (local-set-key (kbd "M-p") 'zk-java-prev-thing)
            (local-set-key (kbd "M-a") 'zk-java-beginning-braces-block)
            (local-set-key (kbd "M-e") 'zk-java-end-braces-block)))
(global-set-key (kbd "C-x M-f") 'zk-find-src-file-in-project)

; Java stacktrace detection in compilation-mode
(require 'zk-java-stacktrace)
(zk-java-stacktrace-detection-enable)
(global-set-key (kbd "C-x \\") 'compile)
(global-set-key (kbd "C-x |") 'compilation-minor-mode)

(global-set-key [f8] 'revert-buffer)

(defun zk-prev-window()
  "Switch to previous window"
  (interactive)
  (other-window -1))

(global-set-key [f5] 'zk-prev-window)
(global-set-key [f6] 'other-window)
(global-set-key [f7] 'switch-to-buffer)

(global-set-key (kbd "C-c p") 'zk-insert-file-path-of-a-buffer)
(global-set-key (kbd "C-c f") 'zk-open-file-path-from-region)
(define-key minibuffer-local-map (kbd "C-c p") 'zk-minibuffer-insert-current-file-path)

(add-hook 'shell-mode-hook
          (lambda()
            "Make dots part of the word so full paths can be expanded by M+/"
            (make-local-variable 'dabbrev-abbrev-char-regexp)
            (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_\\|\\.")
	    (local-set-key (kbd "C-c e") 'zk-editor-stub-open-file)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-echo-syntactic-information-p t)
 '(case-fold-search nil)
 '(dabbrev-case-replace nil)
 '(font-use-system-font t)
 '(ido-enable-flex-matching nil)
 '(inhibit-startup-screen t)
 '(ns-command-modifier nil)
 '(org-startup-indented t)
 '(fci-rule-character ?│)
 '(tags-revert-without-query t))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'zk-go-to-char)
(global-set-key (kbd "C-f") 'zk-go-to-char-forward)
(global-set-key (kbd "C-b") 'zk-go-to-char-backward)


