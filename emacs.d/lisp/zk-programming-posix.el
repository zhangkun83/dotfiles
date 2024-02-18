(require 'zk)
(require 'zk-project)

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
   (list (completing-read "Find a src file: "
                          (-map 'zk-project-get-relative-path
                                (process-lines "bash" "-c" (concat "cat '" zk-project-index-path "/SRCFILES'; echo -n"))))))
  (find-file (zk-project-restore-absolute-path f)))

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
	 (completing-read "Insert import: "
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
  (push-mark)
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

(defun zk-java-enter-argument-list()
  "Move into an argument list and place the point at the
beginning of the first argument."
  (interactive)
  (zk-escape-string)
  (let ((continue-loop-p t))
    (while continue-loop-p
      (down-list)
      ;; We may have entered a bracelet pair, a bracket pair or a
      ;; string. In that case, jump out and skip over that pair.
      (if (eq (char-before) ?\()
          (setq continue-loop-p nil)
        (backward-up-list nil t t)
        (forward-sexp))))
  ;; Skip over all blank characters
  (while (memq (char-after) '(?\t ?\n ?\s ?\r))
    (forward-char)))

(defun zk-java-next-argument()
  "Move to the beginning of the next argument in an argument list"
  (interactive)
  (zk-java-move-to-argument 'forward))

(defun zk-java-prev-argument()
  "Move to the beginning of the previous argument in an argument list"
  (interactive)
  (zk-java-move-to-argument 'backward))

(defun zk-java-move-to-argument (dir)
  "Move to the beginning of an argument. dir can be either 'forward
or 'backward"
  ;; Check if is in a pair of parentheses.
  (save-excursion
    (backward-up-list nil t t)
    (unless (eq (char-after) ?\()
      (user-error "Not in an argument list")))
  (let ((continue-loop-p t) (last-point (point)))
    (while continue-loop-p
      (cond ((eq dir 'forward)
             ;; forward-sexp throw an error at the end of the list
             (ignore-errors
               (forward-sexp)))
            ((eq dir 'backward)
             ;; backward-sexp throw an error at the beginning of the list
             (ignore-errors
               ;; Two backwards then one forward so that the point
               ;; can arrive at the comma.
               (backward-sexp)
               (backward-sexp)
               (forward-sexp))))
      (if (or (eq (char-after) ?,)
              (= (point) last-point))
          (setq continue-loop-p nil))
      (setq last-point (point))))
  ;; The point may have arrived at a comma, skip it and all blank
  ;; characters so that the point arrive at the beginning of the next
  ;; argument
  (while (memq (char-after) '(?, ?\t ?\n ?\s ?\r))
    (forward-char)))

(require 'compile)
(defvar zk-diff-navigate--history nil)
(defun zk-diff-navigate (&optional command)
  "Generic diff navigation with compilation mode."
  (interactive)
  (save-some-buffers)
  (let ((diff-command (if command command
                        (read-string "Diff command: "
                                     (car zk-diff-navigate--history)
                                     '(zk-diff-navigate--history . 1))))
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

(setenv "EDITOR" "~/.emacs.d/bin/editor-stub")
(setenv "PAGER" "cat")
(setenv "P4EDITOR" "~/.emacs.d/bin/editor-stub")
(setenv "P4DIFF" "diff -u")  ; works with zk-diff-navigate
(setenv "G4PENDINGSTYLE" "relativepath")

(add-hook 'java-mode-hook
	  (lambda()
	    "Register my own shortcuts for Java mode"
	    (local-set-key (kbd "C-c i") 'zk-insert-java-import)
	    (local-set-key (kbd "C-c d") 'zk-c-toggle-syntactic-indentation)
            (local-set-key (kbd "M-;") 'recenter-top-bottom)
            (local-set-key (kbd "M-h") 'zk-java-mark-thing)
            (local-set-key (kbd "M-i") 'zk-java-enter-braces-block)
            (local-set-key (kbd "M-o") 'zk-java-exit-bracesblock)
            (local-set-key (kbd "M-n") 'zk-java-next-thing)
            (local-set-key (kbd "M-p") 'zk-java-prev-thing)
            (local-set-key (kbd "C-c TAB") 'zk-java-enter-argument-list)
            (local-set-key (kbd "C-c C-n") 'zk-java-next-argument)
            (local-set-key (kbd "C-c C-p") 'zk-java-prev-argument)
            (local-set-key [backtab] (lambda() (interactive) (c-indent-line-or-region -1)))))

(global-set-key (kbd "C-x \\") 'compile)

(setq compile-command "runp ")

;; go-mode for Golang
(autoload 'go-mode "go-mode" nil t)
(autoload 'gofmt-before-save "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook
          (lambda()
            (setq tab-width 2)))

(add-to-list 'compilation-search-path zk-project-root)
(require 'grep)
(add-to-list 'grep-search-path zk-project-root)

(global-set-key (kbd "C-z f") 'zk-find-src-file-in-project)

(provide 'zk-programming-posix)
