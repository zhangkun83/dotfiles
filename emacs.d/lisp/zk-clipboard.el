(require 'zk)

(defun zk-clipboard-copy (arg)
  "Save the current region (selection) to clipboard using
desktop-helper. With prefix argument (C-u), copy the whole
buffer."
  (interactive "P")
  (unless (or arg mark-active)
    (user-error "Region not active"))
  (let ((buffer (current-buffer))
        (begin (if arg (point-min) (region-beginning)))
        (end (if arg (point-max) (region-end))))
    (if (= begin end)
        (user-error "Content is empty"))
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
  (deactivate-mark))

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
(global-set-key (kbd "C-z C-v") 'zk-clipboard-paste)
(define-key minibuffer-local-map (kbd "C-z C-v") 'zk-clipboard-paste)
(global-set-key (kbd "C-z C-y") 'zk-clipboard-youdao-dict)

(provide 'zk-clipboard)
