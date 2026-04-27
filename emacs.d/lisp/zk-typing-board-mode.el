;;; zk-typing-board-mode.el --- A simple typing board mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kun Zhang

;; Author: Kun Zhang <arthur.kun@gmail.com>

;;; Commentary:

;; A simple major mode based on text-mode. When hitting C-j, the content
;; before the cursor is copied to the clipboard, and a new line is inserted
;; at the beginning of the buffer.

;;; Code:

(require 'zk-clipboard)

(defvar zk-typing-board-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'zk-typing-board-copy-and-insert-line)
    map)
  "Keymap for `zk-typing-board-mode'.")

(defun zk-typing-board-copy-and-insert-line ()
  "Copy content before point to clipboard and insert a new line at buffer start."
  (interactive)
  ;; Mark the content before the cursor as a region.
  (push-mark (point-min) t t)
  (setq mark-active t)
  ;; Send the region to zk-clipboard-copy.
  (zk-clipboard-copy nil)
  ;; Move the cursor to the beginning of the buffer.
  (goto-char (point-min))
  ;; Insert a new line below the cursor (i.e., open a line).
  (open-line 2))

;;;###autoload
(defun zk-typing-board ()
  "Open or switch to the `*typing-board*` buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*typing-board*")))
    (switch-to-buffer buffer)
    (unless (eq major-mode 'zk-typing-board-mode)
      (zk-typing-board-mode))))

;;;###autoload
(define-derived-mode zk-typing-board-mode text-mode "ZK-Typing"
  "Major mode for a simple typing board.
\\{zk-typing-board-mode-map}"
  (setq-local indent-line-function 'indent-relative))

(provide 'zk-typing-board-mode)

;;; zk-typing-board-mode.el ends here
