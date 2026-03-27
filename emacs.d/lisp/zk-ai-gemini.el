;;; zk-ai-gemini.el --- Gemini AI interaction using request.el -*- lexical-binding: t; -*-

(require 'zk)
(require 'request)
(require 'json)

(defvar-local zk-ai-gemini--context-text nil
  "The full context text.")

(defvar-local zk-ai-gemini--history nil
  "The conversation history for the current session buffer.")

(defvar zk-ai-gemini--pending-prompt nil
  "The pending prompt to be sent to a Gemini session.")

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

(defun zk-ai-gemini--create-session-buffer (context-text files &optional buffer-name)
  "Helper to create and initialize a Gemini session buffer."
  (let* ((buf-name (generate-new-buffer-name (or buffer-name "*gemini-session*")))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (ignore-errors (org-mode))
      (setq zk-ai-gemini--context-text context-text)
      (setq zk-ai-gemini--history nil)
      (insert "# Gemini Session\n\n")
      (when files
        (insert "Context Files: " (mapconcat #'zk-abbrev-home-dir-from-path files ", ") "\n\n"))
      (insert "--- Session started ---\n\n"))
    (switch-to-buffer buffer)
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


(defun zk-ai-gemini-new-session (files &optional additional-system-instruction buffer-name)
  "Create a new Gemini session using FILES as context.
This function reads all files and use their content as the context."
  (let ((context-text (zk-ai-gemini--format-files-context files)))
    (setq context-text
          (concat
           "\n**IMPORTANT**: Use org-mode format for all your responses.
Unnumbered lists in the text body uses `-` or `+` as the bullet character.
`*` is reserved for entry headings only.\n\n" context-text))
    (when additional-system-instruction
      (setq context-text
            (concat
             "\n**IMPORTANT**: " additional-system-instruction "\n" context-text)))
    (zk-ai-gemini--create-session-buffer context-text files buffer-name)))

(defun zk-ai-gemini-send (prompt &optional model-level)
  "Send PROMPT to the Gemini session in the current buffer.
MODEL-LEVEL can be 'fast or 'thoughtful. Default is 'fast."
  (interactive "sPrompt: ")
  (unless (boundp 'zk-ai-gemini--history)
    (user-error "Current buffer is not an active Gemini session"))

  (let ((buffer (current-buffer))
        (context-text zk-ai-gemini--context-text)
        (model-name (zk-ai-gemini--get-model (or model-level 'fast))))
    ;; Update UI and history
    (save-excursion
      (goto-char (point-max))
      (insert (format "* User (%s)\n" model-name) prompt "\n\n* Gemini\n"))
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
     :parser (lambda () (buffer-string))
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

(defun zk-ai-gemini-send-pending-prompt (&optional arg)
  "Send the pending prompt to the current Gemini session.
The model level is set to 'thoughtful if a prefix ARG is present, otherwise 'fast."
  (interactive "P")
  (unless zk-ai-gemini--pending-prompt
    (user-error "No pending prompt set"))
  (zk-ai-gemini-send zk-ai-gemini--pending-prompt (if arg 'thoughtful 'fast)))

(defun zk-ai-gemini-generate-prompt-with-region (beg end)
  "Set the pending prompt to a user input followed by the content of the region."
  (interactive "r")
  (let ((user-prompt (read-string "Prompt: ")))
    (setq zk-ai-gemini--pending-prompt
          (concat user-prompt "\n" (buffer-substring-no-properties beg end)))
    (message "Pending prompt set with region content.")))

(defun zk-ai-gemini-start-session-for-region (beg end)
  "Start a new Gemini session with the active region and a user prompt."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((user-prompt (read-string "Prompt: "))
         (region-content (buffer-substring-no-properties beg end))
         (full-prompt (concat user-prompt "\n*Input*:\n" region-content))
         (buf-name (format "*gemini-session* %s"
                           (if (> (length user-prompt) 50)
                               (substring user-prompt 0 50)
                             user-prompt))))
    (zk-ai-gemini-new-session nil nil buf-name)
    (zk-ai-gemini-send full-prompt)))

(provide 'zk-ai-gemini)
;;; zk-ai-gemini.el ends here
