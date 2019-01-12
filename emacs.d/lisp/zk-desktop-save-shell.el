;;; Make desktop-save able to save shell buffers
(defun zk-get-shell-save-dir (desktop-dirname)
  (concat desktop-dirname "/shells/"))

(defun zk-shell-save-desktop-buffer (desktop-dirname)
  "The buffer-saving handler assigned to desktop-save-buffer for shell buffers"
  (let ((dir (zk-get-shell-save-dir desktop-dirname)))
    (progn
      (make-directory dir t)
      (write-region nil nil (concat dir (buffer-name)))))
  default-directory)

(defun zk-shell-restore-desktop-buffer (file-name buffer-name directory)
  "The buffer-restoring handler for shell buffers"
  (setq default-directory directory)
  (let ((saved-bash-args explicit-bash-args)
	(bash-autorun-command (concat "cat '" (zk-get-shell-save-dir desktop-dirname) buffer-name "'; echo")))
    (progn
      (setq explicit-bash-args ; run bash -c "cat '<FILE>'; echo; exec bash --noediting -i"
	    (list "-c" (concat bash-autorun-command "; exec bash --noediting -i")))
      (let ((buffer (shell buffer-name)))
	(erase-buffer)
	(setq explicit-bash-args saved-bash-args)
	buffer)))) ; I don't know how to flawlessly restore the shell session.
	        ; Inserting the text directly to the buffer would make the whole content
	        ; treated as command input, which is a mess.
	        ; Here I just print out the previous content, which is good enough.

(add-hook 'shell-mode-hook
	  (lambda ()
	    (make-local-variable 'desktop-save-buffer)
	    (setq desktop-save-buffer 'zk-shell-save-desktop-buffer)))

(add-to-list 'desktop-buffer-mode-handlers '(shell-mode . zk-shell-restore-desktop-buffer))

(provide 'zk-desktop-save-shell)
