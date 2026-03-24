;;; zk-ai-gemini.el --- Gemini AI interaction using request.el -*- lexical-binding: t; -*-

(require 'zk)
(require 'request)
(require 'json)

(defgroup zk-ai-gemini nil
  "Interacting with Gemini AI through Google's Generative Language API."
  :group 'zk)

(defcustom zk-ai-gemini-api-key (or (getenv "GEMINI_API_KEY") "")
  "API key for Gemini AI. Defaults to the value of the GEMINI_API_KEY environment variable.
Get it from https://aistudio.google.com/app/apikey"
  :type 'string)

(defcustom zk-ai-gemini-model "gemini-2.5-flash"
  "The Gemini model to use (e.g., \"gemini-1.5-flash\" or \"gemini-1.5-pro\").
Context caching requires specific models in the v1beta API."
  :type 'string)

(defcustom zk-ai-gemini-cache-ttl 3600
  "Time-to-live for the cached context in seconds."
  :type 'integer)

(defvar-local zk-ai-gemini--cache-name nil
  "The name of the cached content resource returned by the API.")

(defvar-local zk-ai-gemini--context-text nil
  "The full context text, used if caching is unavailable.")

(defvar-local zk-ai-gemini--history nil
  "The conversation history for the current session buffer.")

(defun zk-ai-gemini--create-session-buffer (cache-name context-text files)
  "Helper to create and initialize a Gemini session buffer."
  (let* ((buf-name (generate-new-buffer-name "*gemini-session*"))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (ignore-errors (markdown-mode))
      (setq zk-ai-gemini--cache-name cache-name)
      (message "XXX: context-text: %s" context-text)
      (setq zk-ai-gemini--context-text context-text)
      (setq zk-ai-gemini--history nil)
      (insert "# Gemini Session\n\n")
      (if cache-name
          (insert "Status: **Cached** (`" cache-name "`)\n")
        (insert "Status: **Uncached** (Fallback mode)\n"))
      (insert "Context Files: " (mapconcat #'zk-abbrev-home-dir-from-path files ", ") "\n\n")
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


(defun zk-ai-gemini-new-session (files)
  "Create a new Gemini session using FILES as context.
This function reads all files, attempts to create a cached context on the
Gemini server. If caching fails (e.g., content too small), it falls back
to sending the full context with each request."
  (interactive (list (dired-get-marked-files)))
  (unless (and zk-ai-gemini-api-key (not (string-empty-p zk-ai-gemini-api-key)))
    (user-error "Please set `zk-ai-gemini-api-key' first"))

  (let ((context-text (zk-ai-gemini--format-files-context files))
        (full-model (if (string-prefix-p "models/" zk-ai-gemini-model)
                        zk-ai-gemini-model
                      (concat "models/" zk-ai-gemini-model))))

    (message "Attempting to create Gemini context cache for %d files..." (length files))
    (request
     (format "https://generativelanguage.googleapis.com/v1beta/cachedContents?key=%s"
             zk-ai-gemini-api-key)
     :type "POST"
     :headers '(("Content-Type" . "application/json"))
     :data (encode-coding-string
            (json-encode
             `((model . ,full-model)
               (contents . [((role . "user")
                             (parts . [((text . ,context-text))]))])
               (ttl . ,(format "%ds" zk-ai-gemini-cache-ttl))))
            'utf-8)
     :parser (lambda () (buffer-string))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (let* ((json-object-type 'alist)
                        (response-data (json-read-from-string data))
                        (cache-name (alist-get 'name response-data)))
                   (zk-ai-gemini--create-session-buffer cache-name context-text files)
                   (message "Gemini session created with cache: %s" cache-name))))
     :error (cl-function
             (lambda (&key data response &allow-other-keys)
               (let ((status (request-response-status-code response)))
                 (message "Caching rejected (Status %s: %s). Falling back to non-caching mode." 
                          status (or data "Empty response"))
                 (zk-ai-gemini--create-session-buffer nil context-text files)))))))

(defun zk-ai-gemini-send (prompt)
  "Send PROMPT to the Gemini session in the current buffer."
  (interactive "sPrompt: ")
  (unless (boundp 'zk-ai-gemini--history)
    (user-error "Current buffer is not an active Gemini session"))

  (let ((buffer (current-buffer))
        (cache-name zk-ai-gemini--cache-name)
        (context-text zk-ai-gemini--context-text)
        (full-model (if (string-prefix-p "models/" zk-ai-gemini-model)
                        zk-ai-gemini-model
                      (concat "models/" zk-ai-gemini-model))))
    ;; Update UI and history
    (save-excursion
      (goto-char (point-max))
      (insert "**User**: " prompt "\n\n**Gemini**: "))
    (push `((role . "user") (parts . [((text . ,prompt))])) zk-ai-gemini--history)
    
    (message "Gemini is thinking...")
    (request
     (format "https://generativelanguage.googleapis.com/v1beta/%s:generateContent?key=%s"
             full-model zk-ai-gemini-api-key)
     :type "POST"
     :headers '(("Content-Type" . "application/json"))
     :data (encode-coding-string
            (json-encode
             (let ((payload `((contents . ,(vconcat (reverse zk-ai-gemini--history))))))
               (if cache-name
                   (setq payload (append `((cachedContent . ,cache-name)) payload))
                 ;; Correct structure for system_instruction: { parts: [{ text: "..." }] }
                 (setq payload (append `((system_instruction . ((parts . [((text . ,context-text))])))) payload)))
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

(provide 'zk-ai-gemini)
;;; zk-ai-gemini.el ends here
