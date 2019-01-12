(add-to-list 'load-path (expand-file-name "~/.emacs.d/evil"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/evil/lib"))

;;;; undo-tree.el is required by evil-mode
(require 'undo-tree)
(global-undo-tree-mode)

;; For neotree.
;; These key bindings conflict with evil-mode. Override them.
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;;;; evil-mode (vim emulation)
(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map "\C-]" 'etags-select-find-tag)
