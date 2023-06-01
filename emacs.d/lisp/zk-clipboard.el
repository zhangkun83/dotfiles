(require 'zk)

(defun zk-clipboard-cut (arg)
  "Delete the current region (selection) and send it to clipboard
using desktop-helper. With prefix argument (C-u), remove line
breaks within paragraphs in the saved content."
  (interactive "P")
  (zk-clipboard-copy arg)
  (delete-region (region-beginning) (region-end)))

(defun zk-clipboard-copy (arg)
  "Save the current region (selection) to clipboard using
desktop-helper. With prefix argument (C-u), remove line breaks
within paragraphs in the saved content."
  (interactive "P")
  (unless mark-active
    (user-error "Region not active"))
  (let ((buffer (current-buffer))
        (begin (region-beginning))
        (end (region-end)))
    (if (= begin end)
        (user-error "No region selected"))
    (if arg (with-temp-buffer
              (insert-buffer-substring buffer begin end)
              (mark-whole-buffer)
              (zk-remove-line-breaks-within-paragraphs-region)
              (zk-clipboard-copy nil))
      (with-temp-buffer
        ;; Use a temp-buffer for client output
        (let ((temp-buffer (current-buffer)))
          (with-current-buffer buffer
            (call-process-region
             begin end
             "desktop-helper-client.py"
             nil temp-buffer nil
             "store-to-clipboard")))
        (message (zk-trim-string (buffer-string)))))
    (deactivate-mark)))

(defun zk-remove-line-breaks-within-paragraphs-region ()
  "Join all lines, except empty lines, within the region.  This
effectively removes all line breaks within paragraphs, making the
text suitable for copying to line-wraping text editors."
  (unless mark-active
    (user-error "Region is not active"))
  (let ((begin (region-beginning))
        (end (region-end)))
    (save-mark-and-excursion
      (goto-char begin)
      ;; Replace every new-line and its adjacent blanks with one space
      (while (search-forward-regexp
              "\\([[:graph:]]\\)[[:blank:]]*\n[[:blank:]]*\\([[:graph:]]\\)" end t)
        (replace-match "\\1 \\2")))))

(defun zk-clipboard-paste ()
  "Retrieve the clipboard from desktop-helper and insert to the current point."
  (interactive)
  ;; Use a temp file for client output that is not the content
  (let ((temp-file (make-temp-file "zk-clipboard-paste")))
    (call-process
     "desktop-helper-client.py"
     nil (list t temp-file) nil
     "retrieve-from-clipboard")
    (with-temp-buffer
      (insert-file-contents temp-file)
      (message (zk-trim-string (buffer-string))))
    (delete-file temp-file)))

(defun zk-clipboard-get-string ()
  "Retrieve the clipboard and return as a string."
  (with-temp-buffer
    (zk-clipboard-paste)
    (buffer-string)))

(defun zk-clipboard-youdao-dict (word)
  "Look up the word in Youdao dictionary. The input is prefilled from clipboard."
  (interactive (list
                (read-string "Youdao: "
                             (zk-clipboard-get-string)
                             'zk-youdao-dict--history)))
  (zk-youdao-dict word))

(global-set-key (kbd "C-z C-c") 'zk-clipboard-copy)
(global-set-key (kbd "C-z C-x") 'zk-clipboard-cut)
(global-set-key (kbd "C-z C-v") 'zk-clipboard-paste)
(define-key minibuffer-local-map (kbd "C-z C-v") 'zk-clipboard-paste)
(global-set-key (kbd "C-z C-y") 'zk-clipboard-youdao-dict)

(provide 'zk-clipboard)
