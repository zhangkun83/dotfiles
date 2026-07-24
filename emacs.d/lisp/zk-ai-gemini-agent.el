;;; zk-ai-gemini-agent.el --- Meeting notes sorting agent using Gemini LLM -*- lexical-binding: t; -*-

(require 'zorg)
(require 'zk-ai-gemini)
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'pulse)

(defgroup zk-ai-gemini-agent nil
  "Meeting notes sorting agent using Gemini."
  :group 'zorg)

(defvar-local zk-ai-gemini-agent--session-active-p nil
  "Non-nil if the current Gemini session is managed by `zk-ai-gemini-agent`.")

;;; Synchronous helper to query Gemini server
(defun zk-ai-gemini-agent--query-gemini (prompt &optional sys-instruct model-level)
  "Send a PROMPT synchronously to Gemini server at localhost:1880 and return response text."
  (let* ((level (or model-level 'fast))
         (model-name (zk-ai-gemini--get-model level))
         (payload `((model . ,model-name)
                    (contents . [((role . "user") (parts . [((text . ,prompt))]))])))
         (json-str (progn
                     (when sys-instruct
                       (setq payload (append `((system_instruction . ((parts . [((text . ,sys-instruct))])))) payload)))
                     (encode-coding-string (json-encode payload) 'utf-8)))
         (temp-file (make-temp-file "gemini-req" nil ".json"))
         (output-buf (generate-new-buffer "*zk-gemini-query*"))
         (result-text nil))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (set-buffer-multibyte nil)
            (insert json-str))
          (let ((exit-code
                 (call-process "curl" nil output-buf nil
                               "-s" "-X" "POST"
                               "-H" "Content-Type: application/json"
                               "-d" (concat "@" temp-file)
                               "http://localhost:1880/generateContent")))
            (when (= exit-code 0)
              (with-current-buffer output-buf
                (goto-char (point-min))
                (let* ((json-object-type 'alist)
                       (json-array-type 'list)
                       (json-key-type 'symbol)
                       (resp (json-read-from-string
                              (decode-coding-string (buffer-string) 'utf-8)))
                       (candidates (alist-get 'candidates resp))
                       (first-cand (elt candidates 0))
                       (content (alist-get 'content first-cand))
                       (parts (alist-get 'parts content))
                       (text (alist-get 'text (elt parts 0))))
                  (setq result-text (string-trim text)))))))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (when (buffer-live-p output-buf) (kill-buffer output-buf)))
    result-text))

(defun zk-ai-gemini-agent--strip-code-fences (str)
  "Remove json or markdown code fences from STR."
  (let ((s (string-trim (or str ""))))
    (when (string-prefix-p "```" s)
      (setq s (replace-regexp-in-string "^```[a-zA-Z0-9_-]*\n?" "" s))
      (setq s (replace-regexp-in-string "\n?```$" "" s)))
    (string-trim s)))

(defun zk-ai-gemini-agent--json-get (alist key-name)
  "Extract KEY-NAME (string) safely from json ALIST whether keys are symbols or strings."
  (let ((val (or (cdr (assoc (intern key-name) alist))
                 (cdr (assoc key-name alist)))))
    (if (stringp val) val (and val (format "%s" val)))))

(defun zk-ai-gemini-agent--highlight-sentence-in-buffer (buf start-pos target-text)
  "Move cursor in BUF to TARGET-TEXT (searching from START-POS) and flash a highlight."
  (when (and buf (buffer-live-p buf) (not (string-empty-p target-text)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (or start-pos (point-min)))
        (when (re-search-forward (regexp-quote target-text) nil t)
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (unless noninteractive
              (switch-to-buffer buf)
              (goto-char beg)
              (pulse-momentary-highlight-region beg end)
              (redisplay))))))))

(defun zk-ai-gemini-agent--convert-top-level-headings-to-second-level (text)
  "Replace top-level headings starting with a single asterisk (^\\* ) with 2nd-level headings (^** )."
  (replace-regexp-in-string "^\\*\\([^*]\\|$\\)" "**\\1" text))

(defun zk-ai-gemini-agent--filter-out-top-level-headings (text)
  "Remove any top-level heading lines starting with a single asterisk (^\\*[^*] or ^\\*$)
when copying back from Gemini buffer, as top-level headings are reserved for prompts."
  (let* ((lines (split-string text "\n"))
         (filtered (cl-remove-if (lambda (line) (string-match-p "^\\*\\([^*]\\|$\\)" line)) lines)))
    (string-join filtered "\n")))

;;; Step 1.1: Remove agenda prompt sections directly in buffer
(defun zk-ai-gemini-agent--clean-agenda-prompts-in-buffer (buf start-marker end-marker)
  "If there is a line '- ---- -' under a back reference, remove it and the text above
it up until the back reference after asking the user for confirmation directly in BUF."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char start-marker)
        (while (re-search-forward "^\\s-*RE:" end-marker t)
          (let ((re-beg (line-beginning-position))
                (re-end (line-end-position))
                (re-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                (prompt-beg nil)
                (sep-end nil))
            (forward-line 1)
            (setq prompt-beg (point))
            ;; Look forward for "- ---- -" before next RE: or heading or end-marker
            (while (and (< (point) end-marker)
                        (not (looking-at "^\\s-*RE:"))
                        (not (looking-at "^\\*+"))
                        (not sep-end))
              (if (looking-at "^\\s-*-\\s-+----\\s-+-$")
                  (progn
                    (forward-line 1)
                    (setq sep-end (point)))
                (forward-line 1)))
            (when sep-end
              (let ((prompt-str (buffer-substring-no-properties prompt-beg sep-end)))
                (zk-ai-gemini-agent--highlight-sentence-in-buffer buf re-beg re-line)
                (if (y-or-n-p
                     (format "Under back reference:\n  %s\nRemove agenda prompt text:\n%s\nConfirm deletion of agenda prompts? "
                             re-line prompt-str))
                    (progn
                      (delete-region prompt-beg sep-end)
                      (goto-char prompt-beg))
                  (goto-char sep-end))))))))))

;;; Step 1.2: Write stub back reference for orphan sections directly in buffer
(defun zk-ai-gemini-agent--add-stub-backrefs-in-buffer (buf start-marker end-marker)
  "If a discussion section lacks a back reference in meeting notes,
write a stub back reference (\"RE: <a short description of the subject>\")
without an actual link, and put the section under it directly in BUF."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char start-marker)
        (let ((in-re-p nil)
              (orphan-start nil))
          (while (< (point) end-marker)
            (cond
             ((looking-at "^\\*+")
              (when orphan-start
                (zk-ai-gemini-agent--prompt-and-insert-stub buf orphan-start (point))
                (setq orphan-start nil))
              (setq in-re-p nil)
              (forward-line 1))
             ((looking-at "^\\s-*RE:")
              (when orphan-start
                (zk-ai-gemini-agent--prompt-and-insert-stub buf orphan-start (point))
                (setq orphan-start nil))
              (setq in-re-p t)
              (forward-line 1))
             ((looking-at "^\\s-*$")
              (if in-re-p
                  (forward-line 1)
                (when orphan-start
                  (zk-ai-gemini-agent--prompt-and-insert-stub buf orphan-start (point))
                  (setq orphan-start nil))
                (forward-line 1)))
             (t
              (if in-re-p
                  (forward-line 1)
                (unless orphan-start
                  (setq orphan-start (point)))
                (forward-line 1)))))
          (when orphan-start
            (zk-ai-gemini-agent--prompt-and-insert-stub buf orphan-start end-marker)))))))

(defun zk-ai-gemini-agent--prompt-and-insert-stub (buf beg end)
  "Prompt user and insert stub RE: line before BEG in BUF."
  (with-current-buffer buf
    (let ((orphan-str (string-trim (buffer-substring-no-properties beg end))))
      (when (not (string-empty-p orphan-str))
        (let* ((preview (if (> (length orphan-str) 80)
                            (concat (substring orphan-str 0 77) "...")
                          orphan-str))
               (first-line (buffer-substring-no-properties beg (min end (line-end-position)))))
          (zk-ai-gemini-agent--highlight-sentence-in-buffer buf beg first-line)
          (let* ((subject (read-string
                           (format "Discussion section lacks 'RE:' back reference:\n  \"%s\"\nEnter a short description of the subject for stub back reference: "
                                   preview)))
                 (stub-line (format "RE: %s\n" (string-trim subject))))
            (goto-char beg)
            (insert stub-line)))))))

;;; Step 1.3: Delete back reference if no discussions exist under it directly in buffer
(defun zk-ai-gemini-agent--delete-empty-backrefs-in-buffer (buf start-marker end-marker)
  "If there were no discussions related to a back reference, delete that back reference directly in BUF."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char start-marker)
        (while (re-search-forward "^\\s-*RE:" end-marker t)
          (let ((re-beg (line-beginning-position))
                (re-end (1+ (line-end-position)))
                (has-discussion-p nil))
            (forward-line 1)
            (let ((chk-beg (point)))
              (while (and (< (point) end-marker)
                          (not (looking-at "^\\s-*RE:"))
                          (not (looking-at "^\\*+")))
                (when (not (looking-at "^\\s-*$"))
                  (setq has-discussion-p t))
                (forward-line 1))
              (unless has-discussion-p
                (message "Deleting back reference with no discussion: %s"
                         (buffer-substring-no-properties re-beg (1- re-end)))
                (delete-region re-beg (point))
                (goto-char re-beg)))))))))

;;; Step 1.4 & Additional Req 1 (Gemini-driven language incoherence with entire notes fed as context)
(defun zk-ai-gemini-agent--clarify-missing-info-in-buffer (buf start-marker end-marker)
  "Use Gemini to identify language incoherence feeding the entire notes document as context,
ask user for clarification after moving cursor and flashing highlight on sentence,
use Gemini with full notes context to incorporate user's clarification into notes,
and allow user to [a]ccept, [r]e-answer, or [m]anually fix."
  (let* ((text (with-current-buffer buf
                 (buffer-substring-no-properties start-marker end-marker)))
         (full-notes-context (with-current-buffer buf
                               (buffer-string)))
         (sys-instruct
          "You are an assistant analyzing meeting notes. Identify sentences or bullet points that suffer from language incoherence, such as missing subject (e.g., 'Need to finalize timeline.', 'Merged a PR...', 'Don't think there is much...'), ambiguous pronouns, or incomplete syntax.")
         (prompt
          (format "Here is the full meeting notes file content for background context:\n--- FULL MEETING NOTES DOCUMENT ---\n%s\n--- END FULL DOCUMENT ---\n\nAnalyze the following specific meeting notes entry to be sorted and find every sentence or bullet item that has language incoherence such as missing subject.\nReturn a strict JSON list of objects with keys \"original\" (exact sentence text) and \"question\" (a concise clarification question like 'Who needs to finalize timeline?').\nIf none found, return []. Do not include commentary, only output the JSON array.\n\nMeeting Notes Entry to Analyze:\n%s"
                  full-notes-context text))
         (resp-json (zk-ai-gemini-agent--query-gemini prompt sys-instruct 'fast))
         (clean-json (zk-ai-gemini-agent--strip-code-fences resp-json))
         (items nil))
    (message "Asking Gemini to identify language incoherence (e.g. missing subjects)...")
    (ignore-errors
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol))
        (setq items (json-read-from-string clean-json))))
    
    (when (and (listp items) (> (length items) 0))
      (dolist (item items)
        (when (listp item)
          (let* ((orig (string-trim (or (zk-ai-gemini-agent--json-get item "original") "")))
                 (question (string-trim (or (zk-ai-gemini-agent--json-get item "question") ""))))
            (when (not (string-empty-p orig))
              ;; Check if orig exists in current region of buffer
              (let ((found-pos nil))
                (with-current-buffer buf
                  (save-excursion
                    (goto-char start-marker)
                    (when (re-search-forward (regexp-quote orig) end-marker t)
                      (setq found-pos (match-beginning 0)))))
                (when found-pos
                  (let ((done nil))
                    (while (not done)
                      ;; Move cursor to sentence in question and flash a highlight
                      (zk-ai-gemini-agent--highlight-sentence-in-buffer buf start-marker orig)
                      (let ((user-ans (read-string
                                       (format "Language incoherence identified in:\n  \"%s\"\nQuestion: %s\nProvide clarification (or press ENTER to skip): "
                                               orig question))))
                        (if (string-empty-p (string-trim user-ans))
                            (progn
                              (message "Skipped clarification for: \"%s\"" orig)
                              (setq done t))
                          (message "Using Gemini with full notes context to incorporate clarification...")
                          (let* ((latest-full-context (with-current-buffer buf (buffer-string)))
                                 (incorp-prompt
                                  (format "Here is the full meeting notes document for background context:\n--- FULL MEETING NOTES DOCUMENT ---\n%s\n--- END FULL DOCUMENT ---\n\nOriginal sentence to fix:\n\"%s\"\n\nUser clarification:\n\"%s\"\n\nUsing the full meeting notes context above to determine missing subjects, project names, or pronouns, incorporate the user's clarification into the original sentence to fix language incoherence. Return ONLY the single revised sentence line."
                                          latest-full-context orig user-ans))
                                 (revised-line (zk-ai-gemini-agent--query-gemini
                                                incorp-prompt
                                                "You are an editor updating meeting notes."
                                                'fast)))
                            (when revised-line
                              (setq revised-line (string-trim (replace-regexp-in-string "^```.*" "" revised-line)))
                              (let ((choice-done nil))
                                (while (not choice-done)
                                  (zk-ai-gemini-agent--highlight-sentence-in-buffer buf start-marker orig)
                                  (let ((choice (read-char-choice
                                                 (format "Original:\n  \"%s\"\nProposed revision:\n  \"%s\"\n[a]ccept; [r]e-answer; [m]anually fix; [s]kip: "
                                                         orig revised-line)
                                                 '(?a ?y ?r ?m ?s ?n))))
                                    (cond
                                     ((or (eq choice ?a) (eq choice ?y))
                                      (with-current-buffer buf
                                        (let ((inhibit-read-only t))
                                          (save-excursion
                                            (goto-char start-marker)
                                            (when (re-search-forward (regexp-quote orig) end-marker t)
                                              (replace-match revised-line t t)
                                              (message "Updated sentence in buffer (unsaved).")))))
                                      (setq choice-done t
                                            done t))
                                     ((eq choice ?r)
                                      ;; Re-answer clarification loop
                                      (setq choice-done t))
                                     ((eq choice ?m)
                                      ;; Manually fix: populate original text in minibuffer
                                      (let ((manual-fix (read-string "Manually edit sentence: " orig)))
                                        (when (not (string-empty-p (string-trim manual-fix)))
                                          (with-current-buffer buf
                                            (let ((inhibit-read-only t))
                                              (save-excursion
                                                (goto-char start-marker)
                                                (when (re-search-forward (regexp-quote orig) end-marker t)
                                                  (replace-match manual-fix t t)
                                                  (message "Applied manual edit in buffer (unsaved)."))))))
                                        (setq choice-done t
                                              done t)))
                                     ((or (eq choice ?s) (eq choice ?n))
                                      (message "Skipped edit for: \"%s\"" orig)
                                      (setq choice-done t
                                            done t)))))))))))))))))))))

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

;;; Step 1.6.2 & Additional Req 2 & Step 1.7: Side-by-side preview, manual adjustment & commit (never saving to disk)
(defun zk-ai-gemini-agent--show-preview-and-commit
    (orig-buf start-marker end-marker generated-entries-text)
  "Show proposed content side-by-side with original org file, allow manual editing,
and commit change directly to original file buffer without saving to disk."
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
      (goto-char start-marker)
      (pulse-momentary-highlight-region start-marker end-marker)
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
        ;; Commit changes directly into original buffer without saving to disk:
        (let ((final-generated-text
               (with-current-buffer preview-buf (buffer-string))))
          (unless noninteractive (delete-other-windows))
          (with-current-buffer orig-buf
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char start-marker)
                ;; Extract original unsorted headline string
                (let* ((orig-headline-line
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
                  ;; Step 1.6.2: Copy generated entry back to original org file buffer,
                  ;; keeping original unsorted entry heading while deleting the content covered by it.
                  (delete-region start-marker end-marker)
                  (goto-char start-marker)
                  ;; Insert the original unsorted entry heading (with empty content)
                  ;; followed by the newly sorted generated entries
                  (insert orig-headline-line "\n\n"
                          (string-trim final-generated-text) "\n")
                  ;; Step 1.7: Delete original unsorted entry heading which has now become empty.
                  (goto-char start-marker)
                  (when (looking-at "^\\*+.*:tbs:")
                    (delete-region (line-beginning-position)
                                   (min (point-max) (1+ (line-end-position))))
                    (message "Deleted empty unsorted entry headline."))))
              ;; NOTE: Explicitly DO NOT call (save-buffer) per requirement: "never save them to disk."
              (message "Successfully sorted and applied meeting notes to buffer (unsaved to disk)!" ))))))))

(defun zk-ai-gemini-agent--on-response-ready (session-buf orig-buf start-marker end-marker)
  "Process Gemini output once response is ready, filtering out top-level headings reserved for prompts."
  (with-current-buffer session-buf
    (save-excursion
      (goto-char (point-max))
      (if (re-search-backward "^\\* Gemini\\s-*$" nil t)
          (let* ((resp-start (match-end 0))
                 (raw-resp-text (string-trim (buffer-substring-no-properties resp-start (point-max))))
                 ;; Avoid copying top-level headings (* ...) as they are reserved for prompts
                 (resp-text (zk-ai-gemini-agent--filter-out-top-level-headings raw-resp-text)))
            ;; Step 1.6.1: Go over each generated entry & fix back references
            (let ((resolved-text (zk-ai-gemini-agent--resolve-generated-backrefs resp-text)))
              ;; Step 1.6.2 & Additional Req 2 & Step 1.7: Side-by-side preview & commit (never saving to disk)
              (zk-ai-gemini-agent--show-preview-and-commit
               orig-buf start-marker end-marker resolved-text)))
        (error "Could not locate Gemini output in session buffer")))))

;;; Step 1.5 & Orchestration: Main agent entry point
;;;###autoload
(defun zk-ai-gemini-agent-sort-meeting-notes ()
  "Automate meeting notes sorting workflow for the zorg framework using Gemini.
Triggered on active region of unsorted meeting notes.
Applies cleanups directly to the original file buffer before inserting into Gemini buffer,
avoids copying top-level headings back from Gemini buffer, and never saves edited files to disk."
  (interactive)
  (unless (use-region-p)
    (user-error "Please select the meeting notes entry to be sorted as an active region."))
  (let* ((orig-buf (current-buffer))
         (start-marker (copy-marker (region-beginning)))
         (end-marker (copy-marker (region-end) t)))
    (deactivate-mark)
    (message "Starting Gemini Meeting Notes Sorting Agent...")
    
    ;; Directly apply cleanups to the original file buffer before inserting into Gemini buffer:
    ;; Step 1.1: Remove agenda prompts (- ---- -) directly in buffer with user confirmation
    (zk-ai-gemini-agent--clean-agenda-prompts-in-buffer orig-buf start-marker end-marker)
    
    ;; Step 1.2: Add stub back reference for orphan sections directly in buffer
    (zk-ai-gemini-agent--add-stub-backrefs-in-buffer orig-buf start-marker end-marker)
    
    ;; Step 1.3: Delete back references with no discussion left directly in buffer
    (zk-ai-gemini-agent--delete-empty-backrefs-in-buffer orig-buf start-marker end-marker)
    
    ;; Step 1.4 & Additional Req 1: Language coherence & ask user for clarification directly applied to buffer
    (zk-ai-gemini-agent--clarify-missing-info-in-buffer orig-buf start-marker end-marker)
    
    ;; Now extract the cleaned-up notes directly from the modified original file buffer:
    (let ((raw-notes (with-current-buffer orig-buf
                       (buffer-substring-no-properties start-marker end-marker))))
      
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
        
        ;; Convert top-level headings (* ...) in cleaned notes to 2nd-level headings (** ...)
        (let ((notes-for-gemini
               (zk-ai-gemini-agent--convert-top-level-headings-to-second-level raw-notes)))
          ;; Insert the pre-processed notes for sorting into Gemini session buffer
          (goto-char (point-max))
          (insert "\nConsider the following meeting notes entry to be sorted:\n"
                  "#+begin_src org\n"
                  notes-for-gemini
                  "\n#+end_src\n"))
        
        ;; Call "C-j" to send prompt to Gemini for main sorting
        (message "Sending prompt to Gemini with C-j...")
        (call-interactively 'zk-ai-gemini-send)
        
        ;; Wait for Gemini response completion asynchronously
        (if noninteractive
            (progn
              (while (with-current-buffer session-buf (eq zk-ai-gemini--state 'waiting-for-response))
                (accept-process-output nil 0.5))
              (zk-ai-gemini-agent--on-response-ready session-buf orig-buf start-marker end-marker))
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
                            session-buf orig-buf start-marker end-marker)))))))))))))

;; Bind C-c z s in zk-zorg-keymap-base
(when (boundp 'zk-zorg-keymap-base)
  (define-key zk-zorg-keymap-base (kbd "C-c z s") 'zk-ai-gemini-agent-sort-meeting-notes))

(provide 'zk-ai-gemini-agent)
;;; zk-ai-gemini-agent.el ends here
