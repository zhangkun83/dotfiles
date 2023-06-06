(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(global-unset-key (kbd "C-z"))

(require 'kotlin-mode)
(require 'zk)

;; Disable tool-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Disable scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable menu bar	
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Make cursor blink forever
(setq blink-cursor-blinks 0)

;; Set default browser to chrome
(setq browse-url-browser-function 'zk-browse-url)

(setq echo-keystrokes 0.01)

;; Treat camelCase as multiple words instead of one
(global-subword-mode)

;;;; Don't enable semantic (semantic doesn't work if Java file contains generics)
;;(semantic-mode 1)

(unless (eq system-type 'windows-nt)
  (require 'zk-project)
  (require 'zk-clipboard)
  (require 'zk-google3))

;; Windows doesn't come with ispell program. Our best bet is the one
;; from cygwin.
(when (eq system-type 'windows-nt)
  (setq ispell-program-name (concat zk-user-home-dir "/cygwin/bin/aspell")))

(require 'vertico)
(vertico-mode t)
(setq completion-styles '(basic substring)
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

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
            ;; Use whitespace-mode to visualize characters exceeding fill-column
            (whitespace-mode t)
            ))

;;; Tramp related
(add-hook 'before-save-hook `zk-save-local-copy-if-remote-file)
(setq tramp-default-method "ssh")

;; Completely disable left-right split
(setq split-width-threshold nil)

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

(setq frame-title-format '("" command-line-default-directory " - emacs"))

;; Don't create backup files at all
(setq make-backup-files nil)

(defun zk-prev-window()
  "Switch to previous window"
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x C-p") 'zk-prev-window)
(global-set-key (kbd "C-x C-n") 'other-window)
(global-set-key (kbd "C-x C-b") 'zk-switch-to-other-buffer)
(global-set-key (kbd "C-x C-l") 'zk-recenter-top-bottom-other-window)
(require 'transpose-frame)
(global-set-key (kbd "C-x 9") 'transpose-frame)
(require 'goto-last-change)
(global-set-key (kbd "C-x C-\\") 'goto-last-change-with-auto-marks)
(global-set-key (kbd "C-z r") 'revert-buffer)
(global-set-key (kbd "C-z p") 'zk-copy-buffer-file-path)
(global-set-key (kbd "C-z b") 'browse-url)
(define-key minibuffer-local-map (kbd "C-z p") 'zk-minibuffer-insert-current-file-path)
(define-key minibuffer-local-map (kbd "C-z n") 'zk-minibuffer-insert-current-file-name)
(global-set-key (kbd "C-z s") 'zk-shell)
(global-set-key (kbd "C-z d") 'zk-diff-navigate)
(global-set-key (kbd "C-z y") 'zk-youdao-dict)
(global-set-key (kbd "C-z h") 'global-hl-line-mode)
(global-set-key (kbd "C-M-y") 'zk-yank-to-register)

(add-hook 'shell-mode-hook
          (lambda()
            ;; Make dots part of the word so full paths can be expanded by M+/
            (make-local-variable 'dabbrev-abbrev-char-regexp)
            (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_\\|\\.")
            ;; Make default-directory track the PWD of the shell
            (setq dirtrack-list '("^(\\([^)]*\\))" 1))
            (dirtrack-mode t)
            (local-set-key (kbd "C-c o") 'zk-open-file-path-from-region-or-at-point)
            (local-set-key (kbd "C-c c") 'zk-shell-command-on-file-at-point)
            (local-set-key (kbd "C-c t") 'comint-truncate-buffer)))

;; Set font
(set-face-attribute 'default nil
		    :family "DejaVu Sans Mono"
                    :height (if (<= 1080 (nth 3 (alist-get 'geometry (car (display-monitor-attributes-list)))))
                                ;; High-res displays
                                105
                              ;; Low-res displays
                              125))

(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'chinese-gbk
                    (font-spec :family (cond ((eq system-type 'cygwin) "Microsoft YaHei")
                                             ((eq system-type 'gnu/linux) "Droid Sans Fallback")))
                    nil t))

(add-hook 'dired-mode-hook
          (lambda() (dired-hide-details-mode 1)))

(if (display-graphic-p)
    (load-theme 'leuven t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-echo-syntactic-information-p t)
 '(compilation-scroll-output t)
 '(dabbrev-case-replace nil)
 '(dired-listing-switches "-alo")
 '(explicit-bash-args
   '("--noediting" "--rcfile" "~/.emacs.d/zk-bash-init.sh" "-i"))
 '(fci-rule-character 9474)
 '(global-hl-line-sticky-flag t)
 '(inhibit-startup-screen t)
 '(ns-command-modifier nil)
 '(org-agenda-sticky t)
 '(org-agenda-window-setup 'current-window)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-log-into-drawer t)
 '(org-startup-indented t)
 '(tags-revert-without-query t)
 '(use-dialog-box nil)
 '(whitespace-line-column nil)
 '(whitespace-style '(face lines-tail)))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'zk-go-to-char)
(global-set-key (kbd "C-f") 'zk-go-to-char-forward)
(global-set-key (kbd "C-b") 'zk-go-to-char-backward)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "blue" :foreground "white")))))
