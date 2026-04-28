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
  "Copy the current paragraph to clipboard and insert a new line at buffer start."
  (interactive)
  (when (save-excursion
        (beginning-of-line)
        (looking-at-p "[[:space:]]*$"))
    (user-error "Not at a paragraph"))
  ;; Put point at end of this paragraph, mark at beginning of this
  ;; paragraph.
  (mark-paragraph -1)
  ;; Send the region to zk-clipboard-copy.
  (zk-clipboard-copy nil)
  (unless (bolp)
    ;; mark-paragraph will put point at the new line if there is one.
    ;; If there is no new line, we are at the end of the document
    ;; without a newline, we will add a newline.
    (newline)))


;;;###autoload
(define-derived-mode zk-typing-board-mode text-mode "ZK-Typing"
  "Major mode for a simple typing board.
\\{zk-typing-board-mode-map}"
  (setq-local indent-line-function 'indent-relative))

(provide 'zk-typing-board-mode)

;;; zk-typing-board-mode.el ends here
