;;; zk-ai-gemini-agent.el --- Meeting notes sorting agent using Gemini LLM -*- lexical-binding: t; -*-

(require 'zorg)
(require 'zk-ai-gemini)
(require 'cl-lib)
(require 'subr-x)

(defgroup zk-ai-gemini-agent nil
  "Meeting notes sorting agent using Gemini."
  :group 'zorg)

(defvar zk-ai-gemini-agent--incomplete-sentence-regex
  "\\(?:^\\s-*-\\s-*\\|\\b\\)\\(?:Need to\\|Don't think\\|Didn't think\\|Implementing\\|Merged\\|Will start\\|Asked\\|Checked\\|Fixed\\|Created\\|Updated\\|Reviewed\\|Approved\\|Synced with\\|Sent\\|Filed\\|Discussed\\|Suggested\\|Noticed\\)\\b"
  "Regex to identify shorthand notes lines likely missing a subject or info.")

(defvar-local zk-ai-gemini-agent--session-active-p nil
  "Non-nil if the current Gemini session is managed by `zk-ai-gemini-agent`.")

;;; Step 1.1: Remove agenda prompt sections under back references
(defun zk-ai-gemini-agent--clean-agenda-prompts (text)
  "If there is a line '- ---- -' under a back reference, remove it and the text above
it up until the back reference after asking the user for confirmation."
  (let* ((lines (split-string text "\n"))
         (new-lines nil)
         (i 0)
         (len (length lines)))
    (while (< i len)
      (let ((line (nth i lines)))
        (if (string-match-p "^\\s-*RE:" line)
            (let ((re-line line)
                  (j (1+ i))
                  (sep-idx nil))
              ;; Look forward for "- ---- -" before next RE: or heading
              (while (and (< j len)
                          (not (string-match-p "^\\s-*RE:" (nth j lines)))
                          (not (string-match-p "^\\*+" (nth j lines))))
                (when (string-match-p "^\\s-*-\\s-+----\\s-+-$" (nth j lines))
                  (setq sep-idx j))
                (setq j (1+ j)))
              (if (and sep-idx
                       (let ((prompt-lines (cl-subseq lines (1+ i) (1+ sep-idx))))
                         (y-or-n-p
                          (format "Under back reference:\n  %s\nRemove agenda prompt text:\n%s\nConfirm deletion of agenda prompts? "
                                  re-line
                                  (mapconcat #'identity prompt-lines "\n")))))
                  (progn
                    ;; Keep the RE: line, skip agenda prompts up to and including "- ---- -"
                    (push re-line new-lines)
                    (setq i (1+ sep-idx)))
                ;; No agenda separator or user rejected: keep as is
                (push line new-lines)
                (setq i (1+ i))))
          (push line new-lines)
          (setq i (1+ i)))))
    (string-join (nreverse new-lines) "\n")))

;;; Step 1.2: Write stub back reference for orphan sections
(defun zk-ai-gemini-agent--add-stub-backrefs (text)
  "If a discussion section lacks a back reference in meeting notes,
write a stub back reference (\"RE: <a short description of the subject>\")
without an actual link, and put the section under it."
  (let* ((lines (split-string text "\n"))
         (new-lines nil)
         (in-re-section-p nil)
         (orphan-buffer nil))
    (cl-flet ((flush-orphan ()
                (when orphan-buffer
                  (let ((orphan-str (string-trim (string-join (nreverse orphan-buffer) "\n"))))
                    (when (not (string-empty-p orphan-str))
                      (let* ((preview (if (> (length orphan-str) 80)
                                          (concat (substring orphan-str 0 77) "...")
                                        orphan-str))
                             (subject (read-string
                                       (format "Discussion section lacks 'RE:' back reference:\n  \"%s\"\nEnter a short description of the subject for stub back reference: "
                                               preview)))
                             (stub-line (format "RE: %s" (string-trim subject))))
                        (push stub-line new-lines)
                        (dolist (l (nreverse orphan-buffer))
                          (push l new-lines)))))
                  (setq orphan-buffer nil))))
      (dolist (line lines)
        (cond
         ((string-match-p "^\\*+" line) ; Entry heading
          (flush-orphan)
          (setq in-re-section-p nil)
          (push line new-lines))
         ((string-match-p "^\\s-*RE:" line) ; Back reference
          (flush-orphan)
          (setq in-re-section-p t)
          (push line new-lines))
         ((string-empty-p (string-trim line))
          (if in-re-section-p
              (push line new-lines)
            (flush-orphan)
            (push line new-lines)))
         (t
          (if in-re-section-p
              (push line new-lines)
            (push line orphan-buffer)))))
      (flush-orphan)
      (string-join (nreverse new-lines) "\n"))))

;;; Step 1.3: Delete back reference if no discussions exist under it
(defun zk-ai-gemini-agent--delete-empty-backrefs (text)
  "If there were no discussions related to a back reference, delete that back reference."
  (let* ((lines (split-string text "\n"))
         (new-lines nil)
         (i 0)
         (len (length lines)))
    (while (< i len)
      (let ((line (nth i lines)))
        (if (string-match-p "^\\s-*RE:" line)
            (let ((j (1+ i))
                  (has-discussion-p nil))
              (while (and (< j len)
                          (not (string-match-p "^\\s-*RE:" (nth j lines)))
                          (not (string-match-p "^\\*+" (nth j lines))))
                (when (not (string-empty-p (string-trim (nth j lines))))
                  (setq has-discussion-p t))
                (setq j (1+ j)))
              (if has-discussion-p
                  (progn
                    (push line new-lines)
                    (setq i (1+ i)))
                (message "Deleting back reference with no discussion: %s" line)
                (setq i j)))
          (push line new-lines)
          (setq i (1+ i)))))
    (string-join (nreverse new-lines) "\n")))

;;; Step 1.4 & Additional Req 1: Language coherence & clarification for missing info
(defun zk-ai-gemini-agent--clarify-missing-info (text)
  "Scan text for incomplete sentences/bullets missing information (e.g. subject)
and ask user for clarification."
  (let* ((lines (split-string text "\n"))
         (new-lines nil))
    (dolist (line lines)
      (if (and (not (string-match-p "^\\*+" line))
               (not (string-match-p "^\\s-*RE:" line))
               (not (string-empty-p (string-trim line)))
               (string-match-p zk-ai-gemini-agent--incomplete-sentence-regex line))
          (let* ((trimmed (string-trim line))
                 (clarification
                  (read-string
                   (format "Sentence may be missing information (e.g. missing subject):\n  \"%s\"\nProvide clarification (or press ENTER to keep as is): "
                           trimmed))))
            (if (not (string-empty-p (string-trim clarification)))
                (let ((prefix (if (string-match "^\\(\\s-*-\\s-*\\|\\s-*\\)" line)
                                  (match-string 1 line)
                                "")))
                  (push (concat prefix (string-trim clarification)) new-lines))
              (push line new-lines)))
        (push line new-lines)))
    (string-join (nreverse new-lines) "\n")))

;;; Step 1.6.1: Find target entry, call C-c l r to copy back reference, and replace invalid link
(defun zk-ai-gemini-agent--find-and-copy-backref (heading-title)
  "Search org note files for entry with HEADING-TITLE, call 'C-c l r'
\(zk-org-copy-external-reference) to generate/copy back reference."
  (let ((found-marker nil)
        (norm-title (downcase (string-trim (zk-org-neutralize-timestamp heading-title)))))
    (dolist (file-name (zk-zorg-list-note-files))
      (when (not found-marker)
        (let* ((file-path (expand-file-name file-name (zk-zorg-directory)))
               (buf (find-file-noselect file-path)))
          (with-current-buffer buf
            (save-excursion
              (goto-char (point-min))
              (while (and (not found-marker)
                          (re-search-forward "^\\(\\*+\\)\\s-+\\(.*?\\)\\s-*$" nil t))
                (let* ((raw-h (match-string-no-properties 2))
                       (clean-h (downcase (string-trim
                                           (zk-org-neutralize-timestamp
                                            (replace-regexp-in-string ":[a-zA-Z0-9_@#:]+:$" "" raw-h))))))
                  (when (or (string= clean-h norm-title)
                            (string-match-p (regexp-quote norm-title) clean-h)
                            (string-match-p (regexp-quote clean-h) norm-title))
                    (setq found-marker (copy-marker (line-beginning-position)))))))))))
    (when found-marker
      (with-current-buffer (marker-buffer found-marker)
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (marker-position found-marker))
            ;; Call C-c l r to create and copy external reference
            (call-interactively 'zk-org-copy-external-reference)
            (car kill-ring)))))))

(defun zk-ai-gemini-agent--resolve-generated-backrefs (response-text)
  "Go over generated entries from RESPONSE-TEXT. If a new back reference is added
without a valid CUSTOM_ID link, search heading text to locate entry, run C-c l r,
and replace invalid link with created link."
  (let* ((lines (split-string response-text "\n"))
         (new-lines nil))
    (dolist (line lines)
      (if (string-match "^\\(\\s-*RE:\\s-+\\)\\(.*\\)$" line)
          (let ((prefix (match-string 1 line))
                (rest (string-trim (match-string 2 line))))
            ;; Check if link has valid [[file:...::#...][^]] structure
            (if (string-match-p "\\[\\[file:[^][]+::#[^][]+\\]\\[\\^\\]\\]" rest)
                (push line new-lines)
              ;; Missing or invalid custom-id link
              (let* ((title-only (replace-regexp-in-string "\\[\\[.*\\]\\]" "" rest))
                     (title-clean (string-trim (replace-regexp-in-string "\\[\\^?\\]" "" title-only)))
                     (created-ref (zk-ai-gemini-agent--find-and-copy-backref title-clean)))
                (if created-ref
                    (progn
                      (message "Replaced invalid back reference '%s' with '%s'" rest created-ref)
                      (push (concat prefix created-ref) new-lines))
                  (push line new-lines)))))
        (push line new-lines)))
    (string-join (nreverse new-lines) "\n")))

;;; Step 1.6.2 & Additional Req 2 & Step 1.7: Side-by-side preview, manual adjustment & commit
(defun zk-ai-gemini-agent--show-preview-and-commit
    (orig-buf orig-start orig-end generated-entries-text)
  "Show proposed content side-by-side with original org file, allow manual editing,
and commit change after user instruction."
  (let* ((preview-buf-name "*Meeting Notes Sorting Preview*")
         (preview-buf (get-buffer-create preview-buf-name)))
    (with-current-buffer preview-buf
      (erase-buffer)
      (org-mode)
      (insert generated-entries-text)
      (goto-char (point-min)))
    
    (unless noninteractive
      (delete-other-windows)
      (switch-to-buffer orig-buf)
      (goto-char orig-start)
      (pulse-momentary-highlight-region orig-start orig-end)
      (let ((preview-win (split-window-right)))
        (select-window preview-win)
        (switch-to-buffer preview-buf)))

    (let ((user-confirmed nil)
          (user-cancelled nil))
      (while (not (or user-confirmed user-cancelled))
        (let ((ans (read-char-choice
                    "Side-by-side preview active in right window. [c]ommit changes; [e]dit in preview window then re-prompt; [a]bort"
                    '(?c ?e ?a))))
          (cond
           ((eq ans ?c) (setq user-confirmed t))
           ((eq ans ?a) (setq user-cancelled t))
           ((eq ans ?e)
            (message "Make your edits in *Meeting Notes Sorting Preview*, then type M-x zk-ai-gemini-agent-commit-preview or press 'C-c C-c'")
            (local-set-key (kbd "C-c C-c")
                           (lambda () (interactive) (throw 'zk-agent-preview-done 'commit)))
            (local-set-key (kbd "C-c C-k")
                           (lambda () (interactive) (throw 'zk-agent-preview-done 'abort)))
            (let ((action (catch 'zk-agent-preview-done
                            (recursive-edit))))
              (cond
               ((eq action 'commit) (setq user-confirmed t))
               ((eq action 'abort) (setq user-cancelled t))))))))
      
      (if user-cancelled
          (progn
            (unless noninteractive (delete-other-windows))
            (message "Meeting notes sorting cancelled by user."))
        ;; Commit changes:
        (let ((final-generated-text
               (with-current-buffer preview-buf (buffer-string))))
          (unless noninteractive (delete-other-windows))
          (with-current-buffer orig-buf
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char orig-start)
                ;; Extract original unsorted headline string
                (let* ((orig-headline-line
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
                  ;; Step 1.6.2: Copy generated entry back to original org file,
                  ;; keeping original unsorted entry heading while deleting the content covered by it.
                  (delete-region orig-start orig-end)
                  (goto-char orig-start)
                  ;; Insert the original unsorted entry heading (with empty content)
                  ;; followed by the newly sorted generated entries
                  (insert orig-headline-line "\n\n"
                          (string-trim final-generated-text) "\n")
                  ;; Step 1.7: Delete original unsorted entry heading which has now become empty.
                  ;; We move back to original headline and remove it so that only the sorted entries remain.
                  (goto-char orig-start)
                  (when (looking-at "^\\*+.*:tbs:")
                    (delete-region (line-beginning-position)
                                   (min (point-max) (1+ (line-end-position))))
                    (message "Deleted empty unsorted entry headline."))))
              (save-buffer)
              (message "Successfully sorted and committed meeting notes entries!"))))))))

(defun zk-ai-gemini-agent--on-response-ready (session-buf orig-buf reg-start reg-end)
  "Process Gemini output once response is ready."
  (with-current-buffer session-buf
    (save-excursion
      (goto-char (point-max))
      (if (re-search-backward "^\\* Gemini\\s-*$" nil t)
          (let* ((resp-start (match-end 0))
                 (resp-text (string-trim (buffer-substring-no-properties resp-start (point-max)))))
            ;; Step 1.6.1: Go over each generated entry & fix back references
            (let ((resolved-text (zk-ai-gemini-agent--resolve-generated-backrefs resp-text)))
              ;; Step 1.6.2 & Additional Req 2 & Step 1.7: Side-by-side preview & commit
              (zk-ai-gemini-agent--show-preview-and-commit
               orig-buf reg-start reg-end resolved-text)))
        (error "Could not locate Gemini output in session buffer")))))

;;; Step 1.5 & Orchestration: Main agent entry point
;;;###autoload
(defun zk-ai-gemini-agent-sort-meeting-notes ()
  "Automate meeting notes sorting workflow for the zorg framework using Gemini.
Triggered on active region of unsorted meeting notes."
  (interactive)
  (unless (use-region-p)
    (user-error "Please select the meeting notes entry to be sorted as an active region."))
  (let* ((orig-buf (current-buffer))
         (reg-start (region-beginning))
         (reg-end (region-end))
         (raw-notes (buffer-substring-no-properties reg-start reg-end)))
    (deactivate-mark)
    (message "Starting Gemini Meeting Notes Sorting Agent...")
    
    ;; Step 1.1: Remove agenda prompts (- ---- -) with user confirmation
    (setq raw-notes (zk-ai-gemini-agent--clean-agenda-prompts raw-notes))
    
    ;; Step 1.2: Add stub back reference for orphan sections
    (setq raw-notes (zk-ai-gemini-agent--add-stub-backrefs raw-notes))
    
    ;; Step 1.3: Delete back references with no discussion left
    (setq raw-notes (zk-ai-gemini-agent--delete-empty-backrefs raw-notes))
    
    ;; Step 1.4 & Additional Req 1: Grammar/coherence & ask user for clarification on missing info
    (setq raw-notes (zk-ai-gemini-agent--clarify-missing-info raw-notes))
    
    ;; Step 1.5: Call C-c z c r to create Gemini session
    (message "Creating Gemini session with C-c z c r...")
    (let ((days (or zk-zorg-ai-gemini-recent-duration-days 180)))
      (zk-zorg-ai-gemini-create-session-with-recent-notes days))
    
    (let ((session-buf (current-buffer)))
      (setq-local zk-ai-gemini-agent--session-active-p t)
      
      ;; Call "C-c z p s" to insert prompt for sorting notes
      (message "Inserting sort prompt with C-c z p s...")
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?s)))
        (call-interactively 'zk-zorg-ai-gemini-insert-prompt))
      
      ;; Insert the pre-processed notes for sorting
      (goto-char (point-max))
      (insert "\nConsider the following meeting notes entry to be sorted:\n"
              "#+begin_src org\n"
              raw-notes
              "\n#+end_src\n")
      
      ;; Call "C-j" to send prompt to Gemini for main sorting
      (message "Sending prompt to Gemini with C-j...")
      (call-interactively 'zk-ai-gemini-send)
      
      ;; Wait for Gemini response completion asynchronously
      (if noninteractive
          (progn
            (while (with-current-buffer session-buf (eq zk-ai-gemini--state 'waiting-for-response))
              (accept-process-output nil 0.5))
            (zk-ai-gemini-agent--on-response-ready session-buf orig-buf reg-start reg-end))
        (let ((timer nil))
          (setq timer
                (run-with-timer
                 0.5 0.5
                 (lambda ()
                   (when (buffer-live-p session-buf)
                     (with-current-buffer session-buf
                       (when (eq zk-ai-gemini--state 'ready)
                         (cancel-timer timer)
                         (message "Gemini response received! Extracting output...")
                         (zk-ai-gemini-agent--on-response-ready
                          session-buf orig-buf reg-start reg-end))))))))))))

;; Bind C-c z s in zk-zorg-keymap-base
(when (boundp 'zk-zorg-keymap-base)
  (define-key zk-zorg-keymap-base (kbd "C-c z s") 'zk-ai-gemini-agent-sort-meeting-notes))

(provide 'zk-ai-gemini-agent)
;;; zk-ai-gemini-agent.el ends here
