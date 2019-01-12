;;; Detection of Java stack traces in compilation-mode and compilation-minor-mode
(defun zk-java-stacktrace-regexp-to-filename ()
  "Generates a relative filename from zk-java-stacktrace-regexp regexp match data."
  (let ((relative-path
         (concat (replace-regexp-in-string "\\." "/" (match-string 1))
                 (match-string 2))))
    (concat zk-project-index-path "/JAVA_LINKS/" relative-path)))

(defun zk-java-stacktrace-detection-enable ()
  "Enable zk's Java stack trace detection for compilation mode.
 This may override existing such functionality from other sources."
  (interactive)
  (add-to-list 'compilation-error-regexp-alist 'java)
  ;; This regex does not only match .java, but all file extensions in similar pattern, e.g., .groovy
  ;; The 'java entry for stacktrace was there initially, but is pretty dumb. We are replacing it.
  (setq compilation-error-regexp-alist-alist
	(assq-delete-all 'java compilation-error-regexp-alist-alist))
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(java .
	  ("[[:space:]]+at \\(\\(?:[[:lower:]]+\\.\\)+\\)[^(]+(\\([[:alnum:]]+\\.[[:lower:]]+\\):\\([[:digit:]]+\\))"
	   zk-java-stacktrace-regexp-to-filename 3)))
  (message "Enabled zk's Java stacktrace detection"))

(provide 'zk-java-stacktrace)
