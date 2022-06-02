(defvar zk-project-root (expand-file-name command-line-default-directory)
  "The root directory of a project. TAGS and SRCFILES are located here.")

(defun zk-set-project-root(f)
  "Set project root where TAGS and SRCFILES are located."
  (interactive "DProject root: ")
  (setq zk-project-root f)
  (message "Project root set as %s" f)
  (setq zk-project-index-path (expand-file-name (concat "~/.zk/index/" zk-project-root))))

(zk-set-project-root zk-project-root)

(defvar zk-grep--history nil)
(defun zk-grep ()
  "Grep through the files from SRCFILES."
  (interactive)
  (let ((pattern (read-string "Grep in src files: "
                              (car zk-grep--history)
                              '(zk-grep--history . 1))))
    (grep-find (concat "zk-grep " pattern))))

(defun zk-grep-current-file--get-file ()
  zk-grep-current-file--file-name)

(defun zk-grep-current-file ()
  "Grep through the current file."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (unless file-name
      (user-error "Current buffer doesn't visit a file"))
    (save-some-buffers)
    (let ((pattern (read-string "Grep in current file:  "
                                 (car zk-grep--history)
                                 '(zk-grep--history . 1)))
          (output_buf (zk-recreate-buffer (concat "*zk-grep:"
                                                  (zk-project-get-relative-path file-name)
                                                  "*"))))
      (with-current-buffer output_buf
        (setq-local zk-grep-current-file--file-name file-name)
        (setq-local compilation-error-regexp-alist '(("^\\([0-9]+\\):.*"
                                                      zk-grep-current-file--get-file
                                                      1)))
        (compilation-minor-mode t)
        (insert "grep \"" pattern "\" " file-name "\n\n")
        ;; This makes the point stays at the top only if the buffer is
        ;; non-empty (made so by the insert above)
        (beginning-of-buffer))
      (display-buffer output_buf)
      (select-window (get-buffer-window output_buf))
      (start-process "zk-grep" output_buf
                     "grep" "-n" pattern (file-name-nondirectory file-name)))))

(require 'dash)
(defun zk-find-src-file-in-project(f)
  "Find a src file indexed in SRCFILES of this project."
  (interactive
   (list (ido-completing-read "Find a src file: "
                              (-map 'zk-project-get-relative-path
                               (process-lines "bash" "-c" (concat "cat '" zk-project-index-path "/SRCFILES'; echo -n"))))))
  (find-file (zk-project-restore-absolute-path f)))

(defun zk-project-get-relative-path(absolute-path)
  "If the absolute path starts with zk-project-root, remove it and make it a relative path"
  (if (string-prefix-p zk-project-root absolute-path)
      (let ((trimmed (substring absolute-path (length zk-project-root))))
        (if (string-prefix-p "/" trimmed)
            (substring trimmed 1)
          trimmed))
    absolute-path))

(defun zk-project-restore-absolute-path(relative-path)
  "If the path is a relative path, add zk-project-root as its prefix"
  (if (string-prefix-p "/" relative-path)
      relative-path
    (concat zk-project-root "/" relative-path)))

(defun zk-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun zk-java-import-line< (line1 line2)
  "Decides whether one java import line should appear before the other.
They must not be equal, and must start with 'import '."
  (let ((p1 (zk-extract-package-name-from-import line1))
        (p2 (zk-extract-package-name-from-import line2)))
    (if (string-prefix-p "import static" line1)
        (if (string-prefix-p "import static " line2)
            (string< p1 p2) t)
      (if (string-prefix-p "import static" line2)
          nil (string< p1 p2)))))

(defun zk-extract-package-name-from-import (line)
  "Extracts a java package name out of the import line, which must start with 'import '."
  (replace-regexp-in-string
   ";.*" "" (replace-regexp-in-string "^import \\(static \\)?" "" line)))

(defun zk-java-identifier-at-point ()
;; sexp includes other non-identifier characters like @ in @Test
;; They must be filtered.
  (let ((id (thing-at-point 'sexp)))
    (if (string-match "[A-Za-z0-9$_]+" id)
        (match-string 0 id)
      id)))

(defun zk-insert-java-import(class-name)
  "Insert an import statement for a Java class."
  (interactive (list
		(let ((default-input (zk-java-identifier-at-point)))
		  (read-string (format "Insert import for (%s): " default-input)
			     nil nil default-input))))
  (let ((result
	 (ido-completing-read "Insert import: "
			  (process-lines "grep" "-F" (concat "." class-name ";") (concat zk-project-index-path "/JAVA_IMPORTS")))))
    (progn
      ;; Find the insertion point
      (push-mark)
      (goto-char (point-min))
      (let ((in-import-zone-p nil)
            (at-insert-point-p nil)
            (current-line nil)
            (prev-line-empty-p t)
            (continue-p t))
	(while continue-p
	  (setq current-line (zk-trim-string (thing-at-point 'line)))
	  (cond ((string= current-line result) ; import already there
                 (progn (setq continue-p nil)
                        (message "Import already exists")))
                ((string-prefix-p "package " current-line)
                 (setq in-import-zone-p t))
                ((string-prefix-p "import " current-line)
                 (when (zk-java-import-line< result current-line)
                   (when (and (not (string-prefix-p "import static " current-line))
                              (string-prefix-p "import static " result))
                     ;; Inserting the last static import. Make sure
                     ;; there is an empty line after it
                     (unless prev-line-empty-p (newline))
                     ;; Insert before the empty line
                     (previous-line))
                   (setq at-insert-point-p t)))
                ((not (string= "" current-line))
                 (when in-import-zone-p
                   ;; First line after the import zone.
                   ;; Make sure there is an empty line at the end of the import zone.
                   (unless prev-line-empty-p (newline))
                   ;; Insert before the empty line
                   (previous-line)
                   (setq at-insert-point-p t))))
	  (if at-insert-point-p
	      (progn
		(insert result)
		(insert "\n")
                (forward-line -1)  ; Place the cursor on the inserted line
		(message "Import inserted: %s" result)
                (save-buffer)
		(setq continue-p nil))
	    (if (= 1 (forward-line 1))
		(setq continue-p nil)))
          (setq prev-line-empty-p (string= "" current-line)))))))

(defun zk-java-at-end-of-thing-p ()
  "Define the end of a java thing, which is a statement ending with ';',
or code block or class/function definitions that end with '}'"
  (or
   (eq ?} (char-before))
   (looking-at-p ";")))

(defun zk-goto-next-non-empty-line ()
  (let ((continue-loop-p t) (last-point -1))
    (while continue-loop-p
      (progn
        (if (string-match-p "[^\s\t\n]+" (thing-at-point 'line t))
            (setq continue-loop-p nil)
          (next-line))
        (if (eq (point) last-point)
            (setq continue-loop-p nil)
          (setq last-point (point)))
        )
      )
    )
  )

(defun zk-java-next-thing ()
  "Move to the next statement, code block or class/function definition"
  (interactive)
  (zk-escape-to-braces)
  (zk-java-move-to-thing
   (lambda ()
     (condition-case nil
         ;; (forward-sexp) may fail if there is no next sexp
         (forward-sexp)))))

(defun zk-java-prev-thing()
  "Move to the previous statement, code block or class/function definition"
  (interactive)
  (zk-escape-to-braces)
  (zk-java-move-to-thing
   (lambda ()
     ;; Moving backward twice and forwarding once makes up always
     ;; stop at the same locations that zk-java-next-thing would
     ;; stop at, which allows us to use the same method to identify
     ;; the end of thing.
     (condition-case nil
         ;; (backward-sexp) may fail if there is no prev sexp
         (progn
           (backward-sexp)
           (backward-sexp)
           (forward-sexp))))))

(defun zk-move-to-next-char (char)
  "Move point to where the char appears next"
  (if (search-forward (char-to-string char) nil t)
      ;; search-forward stops after the char. Move the point to at the char
      (backward-char)))

(defun zk-java-move-to-thing (move-fun)
  "Invoke the move-fun repeatedly until the point arrives at the
next (or previous) statement, code block or class/function
definition (a.k.a. a java thing). The move-fun should eventually
arrive at the end of java thing for this to work."
  ;; Move the point to the end of the current java thing.
  (let ((continue-loop-p t) (last-point (point)))
    (while continue-loop-p
      (condition-case nil
          (funcall move-fun)
        (error (message "Cannot move any further before finding a thing")))
      (if (or (eq (point) last-point)
              (zk-java-at-end-of-thing-p))
          (setq continue-loop-p nil))
      (setq last-point (point)))
    (if (zk-java-at-end-of-thing-p)
        ;; Success! Move the point to the beginning of the next thing.
        (condition-case nil
            (progn
              (forward-sexp)
              (unless (eq (point) last-point)
                (backward-sexp)))
          ;; (forward-sexp) could fail because we are already after the
          ;; last thing in the current braces block. Move to the closing
          ;; brace so that zk-java-mark-thing can select the whole line.
          (error (progn
                   (zk-move-to-next-char ?})
                   (message "Went past the last thing")))))))

(defun zk-escape-string ()
  "Escape the current string if the point is currently in one"
  (let ((parse-state (syntax-ppss)))
    ;; The 4th element in the parse-state indicate the start of
    ;; the current string, or nil if not in.
    (if (nth 3 parse-state)
        (backward-up-list nil t t))))

(defun zk-escape-parens (left-parens-list)
  "Escape the parenthesis listed in left-parens-list"
  (while (let ((left-paren-pos (nth 1 (syntax-ppss))))
           (and left-paren-pos
                (member (char-after left-paren-pos) left-parens-list)
                (progn (backward-up-list nil t t) t)))))

(defun zk-escape-to-braces ()
  "Jump outward until it's not in a string, () or []."
  (zk-escape-string)
  (zk-escape-parens '(?\( ?\[)))

(defun zk-point-start-of-line-p ()
  "Return non-nil if the characters before the point are only white spaces."
  (string-match
   "^[[:blank:]]*$"
   (buffer-substring-no-properties (line-beginning-position) (point))))

(defun zk-java-move-up-over-comment-block ()
  "If point is just after a comment block, move it up to the
beginning of the comment block."
  (interactive)
  (let ((final-point (point)))
    ;; forward-comment goes one comment at a time. A comment block may
    ;; consist of multiple adjacent comments, e.g., multiple
    ;; single-line comments ("//"), or a combination of multi-line
    ;; comments ("/* */") and single-line comments.  We keep moving up
    ;; until we moved over all adjacent comments.
    (while (and
             (forward-comment -1)
             ;; A comment block only consists of comments that occupy
             ;; whole lines, so each move should land the point at the
             ;; start of line. Otherwise, it's a comment that follows
             ;; some code in the same line, and it should not count.
             (zk-point-start-of-line-p))
      ;; When there is no more comment to move over, forward-comment
      ;; returns nil, but may still move the point over some blank
      ;; characters. We don't want the point to move at all in that
      ;; case.
      (setq final-point (point)))
    (goto-char final-point)))

(defun zk-java-mark-thing ()
  "Mark the java thing following the point. Treat if-else,
try-catch-finally constructs as a single thing."
  (interactive)
  (let ((start-of-line-p
         (zk-point-start-of-line-p)))
    ;; Start marking only if the mark is not active, allowing for
    ;; incremental growth of the selection.
    (unless mark-active
      ;; Include leading comment block
      (zk-java-move-up-over-comment-block)
      ;; Start with a clean new-line if possible
      (if start-of-line-p
          (move-beginning-of-line nil))
      (set-mark (point)))
    (zk-java-next-thing)
    (when start-of-line-p
        ;; Continue if an else/catch/finally follows if we started
        ;; from line start, which means if we started selection at
        ;; "if" or "try", include all the else's and catch/finals, but
        ;; if we started selection at "else" or "catch", don't include
        ;; its siblings.
        (while (looking-at-p "[[:blank:]]*\\(else\\([[:blank:]]+if[[:blank:]]*(.*)[[:blank:]]*\\)?\\|catch[[:blank:]]*(.*)[[:blank:]]*\\|finally\\)[[:blank:]]*{")
          (zk-java-next-thing))))
  ;; Exclude trailing comment block
  (zk-java-move-up-over-comment-block)
  ;; Ends at a clean new-line
  (if (zk-point-start-of-line-p)
      (move-beginning-of-line nil)))

(defun zk-java-exit-bracesblock()
  "Exit the current braces block and move point to the beginning"
  (interactive)
  (backward-up-list)
  (while (not (char-equal ?{ (following-char)))
    (backward-up-list))
  ;; Move the point to the beginning of a java thing
  (zk-java-next-thing)
  (zk-java-prev-thing))

(defun zk-java-enter-braces-block ()
  "Enter the next curly braces block"
  (interactive)
  (zk-escape-to-braces)
  (let ((continue-loop-p t)
        (last-point -1)
        (original-point (point)))
    (while continue-loop-p
      ;; If we have just walked past "}", we will always stop right
      ;; after it.  This is much easier than checking whether a "{" is
      ;; ahead.
      (if (eq ?} (char-before))
          (progn
            (backward-sexp)
            (down-list)
            ;; If the rest of the line is empty or only has line
            ;; comments, move to the next line.
            (if (looking-at-p "[\s\t]*\\(//.*\\)?\n")
                (move-beginning-of-line 2))
            (setq continue-loop-p nil))
        (progn
          (ignore-errors
            (forward-sexp))
          (if (eq (point) last-point)
              ;; Failed
              (progn
                (setq continue-loop-p nil)
                (goto-char original-point))
            (setq last-point (point))))))))

(defun zk-copy-buffer-file-path ()
  "Copy the full path of a buffer's file to kill ring"
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer))))
    (kill-new file-name)
    (message "Copied \"%s\"" file-name)))

(defun zk-minibuffer-insert-current-file-path ()
  "Get the full file path of original buffer and insert it to minibuffer."
  (interactive)
  (let ((file-name-at-point
	 (with-current-buffer (window-buffer (minibuffer-selected-window))
	   (buffer-file-name))))
    (when file-name-at-point
      (insert file-name-at-point))))

(defun zk-minibuffer-insert-current-file-name ()
  "Get the file name of original buffer and insert it to minibuffer."
  (interactive)
  (let ((file-name-at-point
	 (with-current-buffer (window-buffer (minibuffer-selected-window))
	   (buffer-file-name))))
    (when file-name-at-point
      (insert (file-name-nondirectory file-name-at-point)))))

(defun zk-editor-stub-open-file ()
  "Open a file whose path is specified before the point following #ZKEDIT#
 which is generated by editor-stub"
  (interactive)
  (let ((original-point (point)))
    (push-mark nil t t)
    (re-search-backward "^#ZKEDIT# ")
    (search-forward "#ZKEDIT# ")
    (let ((path (buffer-substring-no-properties (mark) (point))))
      (pop-mark)
      (goto-char original-point)
      (switch-to-buffer (find-file-noselect path)))))


(defun zk-open-file-path-from-region ()
  "Open a file which path is specified by the region"
  (interactive)
  (if mark-active
      (let ((path (buffer-substring-no-properties (mark) (point))))
        (if (> (length path) 0)
	    (switch-to-buffer (find-file-noselect path))
          (user-error "Selection is empty")))
    (user-error "No selection")))

(defun zk-c-toggle-syntactic-indentation ()
  "Toggle c-syntactic-indentation for current buffer"
  (interactive)
  (setq c-syntactic-indentation (not c-syntactic-indentation))
  (message "Syntactic indentation: %s" (if c-syntactic-indentation "on" "off")))

(defun zk-recenter-top-bottom-other-window ()
  "Call recenter-top-bottom in the other window."
  (interactive)
  (with-selected-window (next-window)
    (recenter-top-bottom)))

(defun zk-save-buffer-as-copy (filename)
  "Save the current buffer to a file as a copy (without visiting the new file)"
  (interactive "F")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename)))

(defun zk-save-remote-file-as-local-copy ()
  "If the current buffer is a remote file, save a local copy for it"
  (interactive)
  (if (or (file-remote-p (buffer-file-name))
          (string-prefix-p "/google/data/rw/" buffer-file-name))
      (let ((basedir "~/.emacs.d/remote-file-copies/"))
        (make-directory basedir t)
        (let ((localfile
               (concat basedir
                       (replace-regexp-in-string "/" "#" (buffer-file-name)))))
          (zk-save-buffer-as-copy localfile)))))

(defun zk-bookmark-set ()
  "Set a bookmark with pre-populated name in zk's custom format."
  (interactive)
  (let ((file-name-at-point (buffer-file-name))
        (line-at-point
         (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (unless file-name-at-point
      (user-error "Current buffer doesn't have a file"))
    (bookmark-set
     (read-string
      "New bookmark: "
      (concat
       (zk-project-get-relative-path file-name-at-point)
       ": "
       (zk-trim-string line-at-point))))))

(defun zk-recreate-buffer (name)
  "Delete the buffer with the given name if it exists, and create one with same name.
   Return the new buffer.  This is preferred to just get-buffer-create because when
   a buffer is reused by the latter compilation-minor-mode stops recognizing error
   lines."
  (ignore-errors (kill-buffer name))
  (get-buffer-create name))

(require 'compile)
(defvar zk-diff-navigate--history nil)
(defun zk-diff-navigate ()
  "Generic diff navigation with compilation mode."
  (interactive)
  (save-some-buffers)
  (let ((diff-command (read-string "Diff command: "
                                   (car zk-diff-navigate--history)
                                   '(zk-diff-navigate--history . 1)))
        (output_buf (zk-recreate-buffer "*ZK Diff Navigation*")))
    (with-current-buffer output_buf
      ;; This is the only expected error line format in this
      ;; buffer. Don't honor any other error regex in this buffer,
      ;; because sometimes the diff output may have a line that looks
      ;; like an error line but it's not.
      (setq-local compilation-error-regexp-alist '(("^\\*\\*\\* \\(.*+\\):\\([0-9]+\\)" 1 2)))
      (compilation-minor-mode t)
      (insert "diff-command: " diff-command "\n\n")
      ;; This makes the point stays at the top only if the buffer is
      ;; non-empty (made so by the insert above)
      (beginning-of-buffer))
    (display-buffer output_buf)
    (select-window (get-buffer-window output_buf))
    ;; DO NOT use shell-command function, because it will display
    ;; output as a message if it's short, and it really messes up the
    ;; display.
    (start-process-shell-command "zk-diff-navigate"
                                 output_buf
                                 (concat "cd " zk-project-root " && "
                                         diff-command " | zk-transform-patch.py"))))

(defun zk-clipboard-kill ()
  "Kill the current region (selection) and send it to clipboard using desktop-helper."
  (interactive)
  (zk-clipboard-save)
  (kill-region (region-beginning) (region-end)))

(defun zk-clipboard-save ()
  "Save the current region (selection) to clipboard using desktop-helper."
  (interactive)
  (if (= (region-beginning) (region-end))
      (user-error "No region selected"))
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      ;; Use a temp-buffer for client output
      (let ((temp-buffer (current-buffer)))
        (with-current-buffer buffer
          (call-process-region
           (region-beginning) (region-end)
           "desktop-helper-client.py"
           nil temp-buffer nil
           "store-to-clipboard")))
      (message (zk-trim-string (buffer-string))))
  (deactivate-mark)))

(defun zk-clipboard-yank ()
  "Retrieve the clipboard from desktop-helper and yank to the current point."
  (interactive)
  ;; Use a temp file for client output that is not the content
  (let ((temp-file (make-temp-file "zk-clipboard-yank")))
    (call-process
     "desktop-helper-client.py"
     nil (list t temp-file) nil
     "retrieve-from-clipboard")
    (with-temp-buffer
      (insert-file-contents temp-file)
      (message (zk-trim-string (buffer-string))))
    (delete-file temp-file)))

(defun zk-clipboard-get-string ()
  "Retrieve the clipboard and return as a string."
  (with-temp-buffer
    (zk-clipboard-yank)
    (buffer-string)))

(defun zk-insert-mean()
  "Take the leading number from the current line and the previous line,
  and insert the mean value of the two as a new line in between.
  Useful for bi-secting version numbers."
  (interactive)
  (previous-line)
  (move-beginning-of-line nil)
  (let ((a (thing-at-point 'number t)))
    (next-line)
    (let ((b (thing-at-point 'number t)))
      (open-line 1)
      (insert (number-to-string (/ (+ a b) 2))))))

(defun zk-browse-url (url &optional _new-window)
  ;; new-window ignored
  (shell-command (concat "desktop-helper-client.py open-url "
                         (prin1-to-string url))))

(defvar zk-shell-command-on-file-at-point--history nil)
(defun zk-shell-command-on-file-at-point ()
  "Run a shell command on the file at the point."
  (interactive)
  (let ((file-name (thing-at-point 'filename)))
    (unless file-name
      (user-error "No filename at point"))
    (let ((command (read-string (concat "FILE: " file-name "\n CMD: ")
                                (car zk-shell-command-on-file-at-point--history)
                                '(zk-shell-command-on-file-at-point--history . 1))))
      (shell-command (concat command " "
                             (prin1-to-string (substring-no-properties file-name)))))))

(when (string-prefix-p "/google/src/cloud" command-line-default-directory)
  (defun zk-google3-find-g4-opened-file(f)
    "Find an opened file in the g4 client"
    (interactive
     (list (ido-completing-read
            "Find an g4 opened file: "
            (-map 'zk-project-get-relative-path
                  (process-lines
                   "bash" "-c"
                   (concat "cd " zk-project-root "; g4 whatsout"))))))
    (find-file (zk-project-restore-absolute-path f)))
  (global-set-key (kbd "C-x g f") 'zk-google3-find-g4-opened-file)

  (defun zk-google3-open-in-codesearch()
    "Open the current file in codesearch and focus on the current line."
    (interactive)
    (browse-url (concat "http://cs/piper///depot/google3/"
            (zk-project-get-relative-path (buffer-file-name))
            ";l="
            (number-to-string (line-number-at-pos)))))
  (global-set-key (kbd "C-x g s") 'zk-google3-open-in-codesearch)

  (defun zk-google3-open-build-sponge-link()
    "Open the sponge link in the compilation buffer."
    (interactive)
    (let ((buffer (get-buffer "*compilation*")))
      (if buffer
          (with-current-buffer (get-buffer "*compilation*")
            (save-excursion
              (goto-char (point-min))
              (when (search-forward "Streaming build results to: http://sponge2")
                (zk-browse-url (thing-at-point 'url)))))
        (message "*compilation* buffer not found"))))
  (global-set-key (kbd "C-x g p") 'zk-google3-open-build-sponge-link)

  (defun zk-google3-open-critique()
    "Open the critique page of the current file."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (unless file-name
        (user-error "Current buffer doesn't visit a file"))
      (shell-command (concat "open-critique-for-file "
                             (prin1-to-string (zk-project-get-relative-path file-name))))))
  (global-set-key (kbd "C-x g c") 'zk-google3-open-critique)

  (defun zk-google3-open-file-from-codesearch-link (link)
    "Open a file indicated by the given codesearch link"
    (interactive (list
                  (read-string "CodeSearch link: " (zk-clipboard-get-string))))
    (string-match "https://[a-z.]*/piper///depot/google3/\\([^;?]+\\)\\(;l=[0-9]+\\)?" link)
    (let* ((path (match-string 1 link))
           (line-substring (match-string 2 link))
           (line (if line-substring
                     (string-to-number (substring line-substring 3)))))
      (if path
          (progn
            (switch-to-buffer (find-file-noselect (zk-project-restore-absolute-path path)))
            (if line (goto-line line)))
        (user-error "Cannot parse the link"))))
  (global-set-key (kbd "C-x g M-f") 'zk-google3-open-file-from-codesearch-link)

  (defun zk-google3-g4-edit ()
    "g4-edit the given path."
    (interactive)
    (let ((output nil)
          (return-status nil)
          (buffer (current-buffer)))
      (with-temp-buffer
        (setq return-status (call-process "g4" nil t nil "edit" (file-name-nondirectory (buffer-file-name buffer))))
        (setq output (zk-trim-string (buffer-string))))
      (if (= 0 return-status)
          (revert-buffer))
        (message output)))
  (global-set-key (kbd "C-x g e") 'zk-google3-g4-edit)
)

(provide 'zk)
