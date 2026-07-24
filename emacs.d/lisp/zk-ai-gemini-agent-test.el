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
                         ((string-match-p "Need to finalize timeline" prompt)
                          "Daniel: Need to finalize timeline.")
                         ((string-match-p "Don't think there is much" prompt)
                          "Daniel: Don't think there is much on the gRPC Go side.")
                         (t "Clarified subject/information"))))
                     ((symbol-function 'read-char-choice)
                      (lambda (prompt _chars)
                        (message "[MOCK read-char-choice] Prompt: %s -> ANSWER: c (commit)" prompt)
                        ?c)))
            
            ;; Synchronously test steps 1.1 - 1.4 first
            (let* ((raw (buffer-substring-no-properties start end))
                   (step1 (zk-ai-gemini-agent--clean-agenda-prompts raw)))
              (message "--- Step 1.1 Output ---\n%s\n-----------------------" step1)
              (unless (not (string-match-p "- ---- -" step1))
                (error "Step 1.1 failed: divider line still present"))
              
              (let ((step2 (zk-ai-gemini-agent--add-stub-backrefs step1)))
                (message "--- Step 1.2 Output ---\n%s\n-----------------------" step2)
                
                (let ((step3 (zk-ai-gemini-agent--delete-empty-backrefs step2)))
                  (message "--- Step 1.3 Output ---\n%s\n-----------------------" step3)
                  (when (string-match-p "Activities in gRPC OSS" step3)
                    (error "Step 1.3 failed: empty back reference was not deleted"))
                  
                  (let ((step4 (zk-ai-gemini-agent--clarify-missing-info step3)))
                    (message "--- Step 1.4 Output ---\n%s\n-----------------------" step4)
                    
                    ;; Now trigger end-to-end command
                    (goto-char start)
                    (set-mark start)
                    (goto-char end)
                    (activate-mark)
                    
                    (message "Calling zk-ai-gemini-agent-sort-meeting-notes...")
                    (zk-ai-gemini-agent-sort-meeting-notes)
                    
                    ;; Wait up to 60 seconds for async Gemini call and side-by-side preview commit
                    (let ((counter 0))
                      (while (and (< counter 120)
                                  (not (string-match-p "Successfully sorted and committed"
                                                        (with-current-buffer "*Messages*" (buffer-string)))))
                        (accept-process-output nil 0.5)
                        (cl-incf counter))
                      (if (< counter 120)
                          (message "=================== TEST PASSED SUCCESSFULLY ===================")
                        (error "Timeout waiting for Gemini agent completion")))))))))))))

(provide 'zk-ai-gemini-agent-test)
