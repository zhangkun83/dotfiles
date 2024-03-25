(require 'org)
(require 'zk-clipboard)

(defun zk-org-export-html-to-clipboard ()
  (interactive)
  (let* ((org-export-show-temporary-export-buffer nil)
         (buffer (org-html-export-as-html)))
    (with-current-buffer buffer
      (zk-clipboard-copy t))))

(provide 'zk-org)
