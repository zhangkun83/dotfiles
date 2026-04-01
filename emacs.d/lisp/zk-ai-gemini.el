;;; zk-ai-gemini.el --- Gemini AI interaction using request.el -*- lexical-binding: t; -*-

(require 'zk)
(require 'zk-org)
(require 'request)
(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defvar-local zk-ai-gemini--context-text nil
  "The full context text.")

(defvar-local zk-ai-gemini--context-files nil
  "List of files currently in the context.")

(defvar-local zk-ai-gemini--history nil
  "The conversation history for the current session buffer.")

(defvar-local zk-ai-gemini--model-level 'fast
  "The model level for the current session (e.g., 'fast or 'thoughtful).")

(defvar-local zk-ai-gemini--state 'ready
  "The current state of the Gemini session ('ready or 'waiting-for-response).")

(defun zk-ai-gemini--find-user-prompt-heading ()
  "Moves point to the top-level heading and checks if it matches \"* User\".
Returns t if found, nil otherwise. Suppresses errors during navigation."
  (ignore-errors
    (zk-org-go-to-top-heading)
    (and (= (org-outline-level) 1)
         (looking-at "^\\* User\\s-*$"))))

(defun zk-ai-gemini--log (msg &optional preserve-user-prompt)
  "Log MSG at the end of the session buffer.  If PRESERVE-USER-PROMPT is
not nil, insert the log before '* User' if it exists."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (if (and preserve-user-prompt
             (zk-ai-gemini--find-user-prompt-heading))
        (progn
          (insert "* " msg "\n")
          (goto-char (point-max)))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " msg "\n"))))

(defun zk-ai-gemini--set-state (state)
  "Set the session STATE and update buffer read-only status."
  (setq zk-ai-gemini--state state)
  (setq buffer-read-only (not (eq state 'ready)))
  (zk-ai-gemini--log (format "State: ~%s~" state))
  (when (eq state 'ready)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (zk-ai-gemini--log "User")))
  (message "Gemini state: %s" state))

(defvar zk-ai-gemini--session-counter 0
  "Counter for Gemini session buffers.")

(defun zk-ai-gemini--get-model (level)
  "Get the model name for LEVEL (fast or thoughtful) from ~/.zk/emacs/ai-models."
  (let* ((config-file (expand-file-name "~/.zk/emacs/ai-models"))
         (models (if (file-exists-p config-file)
                     (with-temp-buffer
                       (insert-file-contents config-file)
                       (split-string (buffer-string) "\n" t))
                   (user-error "Model configuration file %s not found" config-file)))
         (level-str (symbol-name level))
         (model-entry (cl-find-if (lambda (s) (string-prefix-p (concat level-str ":") s))
                                  models)))
    (if model-entry
        (cadr (split-string model-entry ":"))
      (error "Model level %s not found in %s" level config-file))))

(defun zk-ai-gemini--format-files-context (files)
  "Read the content of FILES and format them as a single context string."
  (mapconcat
   (lambda (file)
     (format "--- File: %s ---\n%s\n"
             (zk-abbrev-home-dir-from-path file)
             (with-temp-buffer
               (insert-file-contents file)
               (buffer-string))))
   files
   "\n"))


(defun zk-ai-gemini-new-session (&optional buffer-name-suffix)
  "Create a new Gemini session.
This function initializes a new session with default system instructions."
  (let ((context-text
         (concat
          "\nResponse format requirements:
- Use org-mode format for all your responses. Unnumbered lists in the text body uses `-` or `+` as the bullet character.
- Use ** for first-level heading, *** for second-level heading, and so on.
- Avoid using single `*` for bullet character.
- Use ~ instead of ` to quote inline code.
- Use single * for bold text.
- Use '#+begin_src text' and '#end_src' instead of ``` to quote multi-line code
- Wrap text using width of 80\n")))
    (let* ((buf-name (generate-new-buffer-name
                      (string-trim
                       (format "*zk/ai*<%d> %s"
                               (cl-incf zk-ai-gemini--session-counter)
                               (or buffer-name-suffix "")))))
           (buffer (get-buffer-create buf-name)))
      (with-current-buffer buffer
        (org-mode)
        (let ((map (copy-keymap (current-local-map))))
          (define-key map (kbd "C-j") #'zk-ai-gemini-send)
          (use-local-map map))
        (setq-local org-adapt-indentation nil)
        (setq zk-ai-gemini--context-text context-text)
        (setq zk-ai-gemini--history nil)
        (setq zk-ai-gemini--context-files nil)
        (zk-ai-gemini-set-model-level 'fast)
        (zk-ai-gemini--set-state 'ready))
      (switch-to-buffer-other-window buffer))))

(defun zk-ai-gemini-add-context-file (file)
  "Add FILE to the current Gemini session context.
Do nothing if the file is already in the context."
  (interactive "fAdd context file: ")
  (unless (boundp 'zk-ai-gemini--context-files)
    (user-error "Current buffer is not an active Gemini session"))
  (setq file (expand-file-name file))
  (if (member file zk-ai-gemini--context-files)
      (message "File %s is already in context" (zk-abbrev-home-dir-from-path file))
    (let ((file-context (zk-ai-gemini--format-files-context (list file))))
      (setq zk-ai-gemini--context-text (concat zk-ai-gemini--context-text "\n" file-context))
      (push file zk-ai-gemini--context-files)
      (zk-ai-gemini--log (format "Added context file: ~%s~" (zk-abbrev-home-dir-from-path file)) t))))

(defun zk-ai-gemini-set-model-level (level)
  "Set the model level for the current Gemini session."
  (interactive
   (list (intern (completing-read "Model level: " '("fast" "thoughtful") nil t))))
  (unless (boundp 'zk-ai-gemini--history)
    (user-error "Current buffer is not an active Gemini session"))
  (setq zk-ai-gemini--model-level level)
  (let ((model-name (zk-ai-gemini--get-model level)))
    (zk-ai-gemini--log (format "Model level set to ~%s~ (~%s~)" level model-name) t)
    (message "Model level set to %s (%s)" level model-name)))

(defun zk-ai-gemini-send (&optional prompt)
  "Send PROMPT to the Gemini session in the current buffer.
If PROMPT is nil, use the content after the last '* User' heading."
  (interactive)
  (unless (boundp 'zk-ai-gemini--history)
    (user-error "Current buffer is not an active Gemini session"))
  (unless (eq zk-ai-gemini--state 'ready)
    (user-error "Gemini is already waiting for a response (state: %s)" zk-ai-gemini--state))

  (let ((buffer (current-buffer))
        (context-text zk-ai-gemini--context-text)
        (model-name (zk-ai-gemini--get-model zk-ai-gemini--model-level)))
    ;; Update UI and history
    (if prompt
        (progn
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (goto-char (point-max))
          (insert prompt "\n"))
      ;; Extract from buffer
      (save-excursion
        (goto-char (point-max))
        (if (zk-ai-gemini--find-user-prompt-heading)
            (setq prompt (buffer-substring-no-properties (line-end-position) (point-max)))
          (user-error "Could not find '* User' heading")))
      (setq prompt (string-trim prompt))
      (if (string-empty-p prompt)
          (user-error "Prompt is empty")
        (goto-char (point-max))))

    (push `((role . "user") (parts . [((text . ,prompt))])) zk-ai-gemini--history)
    (zk-ai-gemini--set-state 'waiting-for-response)

    (message "Gemini is thinking...")
    (request
     "http://localhost:1880/generateContent"
     :type "POST"
     :headers '(("Content-Type" . "application/json"))
     :data (encode-coding-string
            (json-encode
             (let ((payload `((model . ,model-name)
                              (contents . ,(vconcat (reverse zk-ai-gemini--history))))))
               (setq payload (append `((system_instruction . ((parts . [((text . ,context-text))])))) payload))
               payload))
            'utf-8)
     :parser (lambda () (decode-coding-string (buffer-string) 'utf-8))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (with-current-buffer buffer
                   (unless (eq zk-ai-gemini--state 'ready)
                     (condition-case err
                         (let* ((json-object-type 'alist)
                                (response-data (json-read-from-string data))
                                (candidates (alist-get 'candidates response-data))
                                (first-candidate (elt candidates 0))
                                (content (alist-get 'content first-candidate))
                                (parts (alist-get 'parts content))
                                (text (alist-get 'text (elt parts 0))))
                           (let ((inhibit-read-only t))
                             (goto-char (point-max))
                             (zk-ai-gemini--log "Gemini")
                             (insert text "\n")
                             (message "Gemini reply received."))
                           (push `((role . "model") (parts . [((text . ,text))])) zk-ai-gemini--history))
                       (error
                        (zk-ai-gemini--log "Error")
                        (let ((inhibit-read-only t))
                          (insert (format "%S\n" err)))))
                     (zk-ai-gemini--set-state 'ready)))))
     :error (cl-function
             (lambda (&key data response &allow-other-keys)
               (let ((status (request-response-status-code response))
                     (err (request-response-error-thrown response)))
                 (with-current-buffer buffer
                   (unless (eq zk-ai-gemini--state 'ready)
                     (goto-char (point-max))
                     (zk-ai-gemini--log "Request failed")
                     (let ((inhibit-read-only t))
                       (insert (cond 
                                (status (format "Status: %d\n" status))
                                (err (format "Network Error: %S\n" err))
                                (t "Unknown Error\n"))
                               (when data (concat "\n" data "\n"))))
                     (zk-ai-gemini--set-state 'ready)))))))))


(defun zk-ai-gemini--escape-src-content (content)
  "Escape CONTENT so it can be safely placed inside an Org-mode src block.
Specifically, it adds a comma before lines starting with '*' or '#+'."  
  (replace-regexp-in-string 
   "^[ \t]*\\(?:\\*\\|#\\+\\)" ; The pattern
   ",\\&"                      ; The replacement (& means 'the whole match')
   content                     ; The source string
   t))                         ; fixedcase

(defun zk-ai-gemini-start-session ()
  "Start a new Gemini session.
If the region is active, use it as context and prompt for an initial
user prompt.  Otherwise, just create a new empty session.  Returns the
session buffer."
  (interactive)
  (if (use-region-p)
      (let* ((region-content (buffer-substring-no-properties
                              (region-beginning) (region-end)))
             (src-buffer-mode major-mode)
             (suffix (concat "region from " (buffer-name)))
             (session-buffer (zk-ai-gemini-new-session suffix)))
        (insert (concat "Consider the following input:\n"
                        "--- Begin of input ---\n"
                        (if (eq src-buffer-mode 'org-mode)
                            region-content
                          (concat "#+begin_src text\n"
                                  (zk-ai-gemini--escape-src-content region-content)
                                  "#+end_src"))
                        "\n--- End of input ---\n"))
        (deactivate-mark)
        session-buffer)
    (zk-ai-gemini-new-session)))

(provide 'zk-ai-gemini)
;;; zk-ai-gemini.el ends here
