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

(require 'zk-project)
(require 'subr-x)
(unless (or
         ;; The following modules don't support native Windows
         (eq system-type 'windows-nt)
         ;; And they don't make sense in the HOME directory
         (string-equal (string-remove-suffix "/" zk-user-home-dir)
                       (string-remove-suffix "/" zk-project-root)))
  (require 'zk-programming-posix)
  (require 'zk-google3))

(unless (eq system-type 'windows-nt)
  ;; Doesn't support native Windows
  (require 'zk-clipboard))

;; Windows doesn't come with ispell program. Our best bet is the one
;; from cygwin.
(when (eq system-type 'windows-nt)
  (setq ispell-program-name (concat zk-user-home-dir "/cygwin/bin/aspell")))

(when (eq window-system 'w32)
  (require 'zk-mswin-scaling)
  (global-set-key (kbd "C-z w s") 'zk-mswin-scaling-scale-default-font-for-monitor))

;; The temp buffer used by eshell to save command history is using
;; latin-1 encoding on Windows and would complain if the history has
;; non-latin characters. This will fix it.
(set-default-coding-systems 'utf-8)

(setq zk-frame-title-base-name (concat "Emacs:" zk-project-root))

(setq completion-styles '(substring)
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

;; Don't create backup files at all
(setq make-backup-files nil)

(defun zk-prev-window()
  "Switch to previous window"
  (interactive)
  (other-window -1))

(defun zk-prev-frame()
  "Switch to previous frame"
  (interactive)
  (other-frame -1))

(global-set-key (kbd "<f7>") 'other-window)
(global-set-key (kbd "M-<f7>") 'zk-prev-window)
(global-set-key (kbd "<f8>") 'other-frame)
(global-set-key (kbd "M-<f8>") 'zk-prev-frame)
(global-set-key (kbd "C-x C-b") 'zk-switch-to-other-buffer)
(global-set-key (kbd "C-x C-l") 'zk-recenter-top-bottom-other-window)
(require 'transpose-frame)
(global-set-key (kbd "C-x 9") 'transpose-frame)
(global-set-key (kbd "C-x 5 3") 'zk-popup-window-to-new-frame)
(global-set-key (kbd "C-z k") 'zk-kill-buffer-and-window-or-frame)
(require 'goto-last-change)
(global-set-key (kbd "C-x C-\\") 'goto-last-change-with-auto-marks)
(global-set-key (kbd "C-z r") 'revert-buffer)
(global-set-key (kbd "C-z p") 'zk-copy-buffer-file-path)
(global-set-key (kbd "C-z u b") 'browse-url)
(global-set-key (kbd "C-z u s") 'zk-shorten-url-at-point)
(global-set-key (kbd "C-z C-b") 'zk-goto-base-buffer)
(define-key minibuffer-local-map (kbd "C-z p") 'zk-minibuffer-insert-current-file-path)
(define-key minibuffer-local-map (kbd "C-z n") 'zk-minibuffer-insert-current-file-name)
(global-set-key (kbd "C-z s") 'shell)
(global-set-key (kbd "C-z d") 'zk-diff-navigate)
(global-set-key (kbd "C-z y") 'zk-youdao-dict)
(global-set-key (kbd "C-z h") 'hl-line-mode)
(global-set-key (kbd "C-z j") 'zk-remove-line-breaks-within-paragraphs)
(global-set-key (kbd "C-z c") 'zk-copy-region-to-temp-buffer)
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
            (local-set-key (kbd "C-c t") 'comint-truncate-buffer)
            ;; shell-mode insists on using slashes to tab-complete paths, while
            ;; Windows doesn't always recognize slashes.  Disable tab-complete.
            (if (eq system-type 'windows-nt)
                (define-key shell-mode-map "\t" 'self-insert-command))))

(add-hook 'compilation-mode-hook
          (lambda()
            (local-set-key (kbd "C-c C-p") 'zk-copy-pid)))

(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'chinese-gbk
                    (font-spec :family (cond ((or (eq system-type 'cygwin)
                                                  (eq system-type 'windows-nt)) "Microsoft YaHei")
                                             ((eq system-type 'gnu/linux) "Droid Sans Fallback")))
                    nil t))

(add-hook 'dired-mode-hook
          (lambda() (dired-hide-details-mode 1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-echo-syntactic-information-p t)
 '(compilation-scroll-output t)
 '(custom-safe-themes
   '("c4cecd97a6b30d129971302fd8298c2ff56189db0a94570e7238bc95f9389cfb" default))
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
 '(switch-to-buffer-obey-display-actions t)
 '(tags-revert-without-query t)
 '(use-dialog-box nil)
 '(whitespace-line-column nil)
 '(whitespace-style '(face lines-tail)))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;
 ;; The "fixed-pitch" face is originally defined in faces.el, with
 ;; family "Monospace".  However, on Windows it means "Courier" which
 ;; is pretty ugly, and is used on code snippets in org-mode with
 ;; themes other than leuven.  Override it to my font of choice.  (Use
 ;; "C-u C-x =" to find out the face used to display the character at
 ;; point).
 '(fixed-pitch ((t (:family zk-font-family))))
 '(region ((t (:background "blue" :foreground "white")))))
