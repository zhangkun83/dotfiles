;;; zk-ai-gemini.el --- Gemini AI interaction using request.el -*- lexical-binding: t; -*-

(require 'zk)
(require 'request)
(require 'json)
(require 'cl-lib)

(defvar-local zk-ai-gemini--context-text nil
  "The full context text.")

(defvar-local zk-ai-gemini--history nil
  "The conversation history for the current session buffer.")

(defvar-local zk-ai-gemini--model-level 'fast
  "The model level for the current session (e.g., 'fast or 'thoughtful).")

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

(defun zk-ai-gemini--create-session-buffer (context-text files &optional buffer-name-suffix)
  "Helper to create and initialize a Gemini session buffer."
  (let* ((buf-name (generate-new-buffer-name
                    (format "*zk/ai*<%d> %s"
                            (cl-incf zk-ai-gemini--session-counter)
                            (or buffer-name-suffix "session"))))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (ignore-errors (org-mode))
      (setq zk-ai-gemini--context-text context-text)
      (setq zk-ai-gemini--history nil)
      (zk-ai-gemini-set-model-level 'fast)
      (insert "# Gemini Session\n\n")
      (when files
        (insert "Context Files: " (mapconcat #'zk-abbrev-home-dir-from-path files ", ") "\n\n"))
      (insert "--- Session started ---\n\n"))
    (switch-to-buffer-other-window buffer)
    buffer))


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


(defun zk-ai-gemini-new-session (files &optional additional-system-instruction buffer-name-suffix)
  "Create a new Gemini session using FILES as context.
This function reads all files and use their content as the context."
  (let ((context-text (zk-ai-gemini--format-files-context files)))
    (setq context-text
          (concat
           "\nResponse format requirements:
- Use org-mode format for all your responses. Unnumbered lists in the text body uses `-` or `+` as the bullet character.
- Use ** for first-level heading, *** for second-level heading, and so on.
- Avoid using single `*` for bullet character.
- Use ~ instead of ` to quote inline code.
- Use '#+begin_src text' and '#end_src' instead of ``` to quote multi-line code
- Wrap text using width of 80" context-text))
    (when additional-system-instruction
      (setq context-text
            (concat
             "\n**IMPORTANT**: " additional-system-instruction "\n" context-text)))
    (zk-ai-gemini--create-session-buffer context-text files buffer-name-suffix)))

(defun zk-ai-gemini-set-model-level (level)
  "Set the model level for the current Gemini session."
  (interactive
   (list (intern (completing-read "Model level: " '("fast" "thoughtful") nil t))))
  (unless (boundp 'zk-ai-gemini--history)
    (user-error "Current buffer is not an active Gemini session"))
  (setq zk-ai-gemini--model-level level)
  (let ((model-name (zk-ai-gemini--get-model level)))
    (goto-char (point-max))
    (insert (format "\n--- Model level set to %s (%s) ---\n\n" level model-name)))
    (message "Model level set to %s (%s)" level model-name))

(defun zk-ai-gemini-send (prompt)
  "Send PROMPT to the Gemini session in the current buffer."
  (interactive "sPrompt: ")
  (unless (boundp 'zk-ai-gemini--history)
    (user-error "Current buffer is not an active Gemini session"))

  (let ((buffer (current-buffer))
        (context-text zk-ai-gemini--context-text)
        (model-name (zk-ai-gemini--get-model zk-ai-gemini--model-level)))
    ;; Update UI and history
    (goto-char (point-max))
    (insert "\n* User\n" prompt "\n\n* Gemini\n")
    (push `((role . "user") (parts . [((text . ,prompt))])) zk-ai-gemini--history)

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
                 (let* ((json-object-type 'alist)
                        (response-data (json-read-from-string data))
                        (candidates (alist-get 'candidates response-data))
                        (first-candidate (elt candidates 0))
                        (content (alist-get 'content first-candidate))
                        (parts (alist-get 'parts content))
                        (text (alist-get 'text (elt parts 0))))
                   (with-current-buffer buffer
                     (save-excursion
                       (goto-char (point-max))
                       (insert text "\n\n")
                       (goto-char (point-max)))
                     (push `((role . "model") (parts . [((text . ,text))])) zk-ai-gemini--history))
                   (message "Gemini reply received."))))
     :error (cl-function
             (lambda (&key data response &allow-other-keys)
               (let ((status (request-response-status-code response)))
                 (message "Gemini request failed (Status %s): %s" 
                          status (or data "No details available"))))))))

(defun zk-ai-gemini-start-session (&optional beg end)
  "Start a new Gemini session.
If BEG and END are provided (e.g., when the region is active), use it as
context and prompt for an initial user prompt.
If BEG and END are nil, just create a new empty session."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (let* ((user-prompt (read-string "Prompt: "))
             (region-content (buffer-substring-no-properties beg end))
             (full-prompt (concat user-prompt "\n*Input*:\n" region-content))
             (suffix (if (> (length user-prompt) 80)
                         (substring user-prompt 0 80)
                       user-prompt)))
        (zk-ai-gemini-new-session nil nil suffix)
        (zk-ai-gemini-send full-prompt))
    (zk-ai-gemini-new-session nil nil "new session")))

(provide 'zk-ai-gemini)
;;; zk-ai-gemini.el ends here
