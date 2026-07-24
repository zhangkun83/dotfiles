;;; zk-ai-gemini-agent-test.el --- Automated test for zk-ai-gemini-agent -*- lexical-binding: t; -*-

(require 'zorg)
(require 'zk-ai-gemini-agent)

(defun zk-ai-gemini-agent-run-test ()
  (message "=================== STARTING INTEGRATION TEST ===================")
  (let* ((notes-file (expand-file-name "notes2026q3.org" (zk-zorg-directory)))
         (buf (find-file-noselect notes-file)))
    (with-current-buffer buf
      (goto-char (point-min))
      (unless (re-search-forward "^\\*\\* Unsorted meeting notes <2026-07-21 Tue>\\s-+:@danielztliu:tbs:" nil t)
        (error "Could not find target tbs entry in notes2026q3.org"))
      (let ((start (line-beginning-position)))
        (org-end-of-subtree t t)
        (let ((end (point)))
          (message "Found target entry from pos %d to %d" start end)
          (set-mark start)
          (goto-char end)
          (activate-mark)
          
          ;; Mock user interaction for y-or-n-p and read-string and read-char-choice
          (cl-letf* ((y-or-n-p-count 0)
                     ((symbol-function 'y-or-n-p)
                      (lambda (prompt)
                        (cl-incf y-or-n-p-count)
                        (message "[MOCK y-or-n-p #%d] Prompt: %s -> ANSWER: y" y-or-n-p-count prompt)
                        t))
                     (read-string-count 0)
                     ((symbol-function 'read-string)
                      (lambda (prompt &optional _initial-input _history _default_value _inherit_input_method)
                        (cl-incf read-string-count)
                        (message "[MOCK read-string #%d] Prompt: %s" read-string-count prompt)
                        (cond
                         ((string-match-p "finalize timeline" prompt)
                          "Daniel needs to finalize timeline.")
                         ((string-match-p "gRPC Go" prompt)
                          "Daniel doesn't think there is much on the gRPC Go side.")
                         (t "Daniel"))))
                     ((symbol-function 'read-char-choice)
                      (lambda (prompt _chars)
                        (message "[MOCK read-char-choice] Prompt: %s" prompt)
                        (if (string-match-p "Side-by-side" prompt) ?c ?a))))
            
            (message "Calling zk-ai-gemini-agent-sort-meeting-notes...")
            (zk-ai-gemini-agent-sort-meeting-notes)
            
            ;; Wait up to 90 seconds for async Gemini call and side-by-side preview commit
            (let ((counter 0))
              (while (and (< counter 180)
                          (not (string-match-p "Successfully sorted and applied"
                                                (with-current-buffer "*Messages*" (buffer-string)))))
                (accept-process-output nil 0.5)
                (cl-incf counter))
              (if (< counter 180)
                  (progn
                    (when (buffer-modified-p buf)
                      (message "Verified: Buffer is modified in memory (never saved to disk)."))
                    (message "=================== TEST PASSED SUCCESSFULLY ==================="))
                (error "Timeout waiting for Gemini agent completion")))))))))

(provide 'zk-ai-gemini-agent-test)
