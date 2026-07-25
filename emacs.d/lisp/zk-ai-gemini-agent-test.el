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

(require 'ert)

(ert-deftest test-zk-ai-gemini-agent-parse-title-and-timestamp ()
  "Test parsing of pre-timestamp prefix and date string from generated RE: headings."
  (let ((p1 (zk-ai-gemini-agent--parse-title-and-timestamp "Weekly Team Sync <2026-07-20 Mon 10:00>"))
        (p2 (zk-ai-gemini-agent--parse-title-and-timestamp "1-on-1 with Bob [2026-07-15]"))
        (p3 (zk-ai-gemini-agent--parse-title-and-timestamp "Project Review (2026-07-18)"))
        (p4 (zk-ai-gemini-agent--parse-title-and-timestamp "Discussion on bug fix")))
    (should (string= (car p1) "Weekly Team Sync"))
    (should (string= (cdr p1) "2026-07-20"))
    (should (string= (car p2) "1-on-1 with Bob"))
    (should (string= (cdr p2) "2026-07-15"))
    (should (string= (car p3) "Project Review"))
    (should (string= (cdr p3) "2026-07-18"))
    (should (string= (car p4) "Discussion on bug fix"))
    (should (null (cdr p4)))))

(ert-deftest test-zk-ai-gemini-agent-resolve-generated-backrefs ()
  "Integration test verifying org-map-entries backref resolution."
  (let* ((temp-dir (make-temp-file "zk-agent-test-" t))
         (file1 (expand-file-name "file1.org" temp-dir))
         (input-text "** Discussion item\n  RE: Weekly Team Sync <2026-07-20 Mon>\n- Notes here"))
    (with-temp-file file1
      (insert "* Unrelated\n** Weekly Team Sync <2026-07-20 Mon>\n:PROPERTIES:\n:CUSTOM_ID: sync-20260720\n:END:\n"))
    (cl-letf (((symbol-function 'zk-zorg-directory) (lambda () temp-dir))
              ((symbol-function 'zk-zorg-list-note-files) (lambda () '("file1.org"))))
      (let ((resolved (zk-ai-gemini-agent--resolve-generated-backrefs input-text)))
        (should (string-match-p "#sync-20260720" resolved))
        (should-not (string-match-p "<2026-07-20 Mon>" resolved))))
    (delete-directory temp-dir t)))

(provide 'zk-ai-gemini-agent-test)
