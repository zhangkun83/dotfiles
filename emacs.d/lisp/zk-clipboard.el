(require 'zk)

(defvar zk-clipboard-backend (if (display-graphic-p) 'CLIPBOARD 'DESKTOP-HELPER)
  "The backend of clipboard operations.  If `CLIPBOARD', will operate the
clipboard from elisp directly.  If `DESKTOP-HELPER', will operate the clipboard
throught dh-client.")

(defun zk-clipboard-copy ()
  "Save the current region (selection) to clipboard."
  (interactive)
  (unless (use-region-p)
    (user-error "Region not active"))
  (let ((buffer (current-buffer))
        (begin (region-beginning))
        (end (region-end)))
    (if (= begin end)
        (user-error "Content is empty"))
    (cond ((eq zk-clipboard-backend 'CLIPBOARD)
           (let ((select-enable-clipboard t))
             (gui-select-text (buffer-substring-no-properties begin end))
             (message "Copied to clipboard.")))
          ((eq zk-clipboard-backend 'DESKTOP-HELPER)
           (with-temp-buffer
             ;; Use a temp-buffer for client output
             (let ((temp-buffer (current-buffer)))
               (with-current-buffer buffer
                 (call-process-region
                  begin end
                  zk-dh-client-path
                  nil temp-buffer nil
                  "write-clip")))
             (message (zk-trim-string (buffer-string))))))
    (deactivate-mark)))

(defun zk-clipboard-paste ()
  "Retrieve the clipboard from the system clipboard (or desktop-helper if
not in graphics mode) and insert to the current point."
  (interactive)
  (cond ((eq zk-clipboard-backend 'CLIPBOARD)
         (insert (or (gui-get-selection 'CLIPBOARD 'STRING)
                     (gui-get-selection 'PRIMARY 'STRING)
                     ""))
         (message "Pasted from clipboard."))
        ((eq zk-clipboard-backend 'DESKTOP-HELPER)
         ;; Use a temp file for client output that is not the content
         (let ((temp-file (make-temp-file "zk-clipboard-paste")))
           (call-process
            zk-dh-client-path
            nil (list t temp-file) nil
            "read-clip")
           (with-temp-buffer
             (insert-file-contents temp-file)
             (message (zk-trim-string (buffer-string))))
           (delete-file temp-file)))))

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
(global-set-key (kbd "C-z C-v") 'zk-clipboard-paste)
(define-key minibuffer-local-map (kbd "C-z C-v") 'zk-clipboard-paste)
(global-set-key (kbd "C-z C-y") 'zk-clipboard-youdao-dict)

(provide 'zk-clipboard)
