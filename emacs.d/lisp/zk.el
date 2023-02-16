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
   (list (completing-read "Find a src file: "
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

(defun zk-copy-buffer-file-path ()
  "Copy the full path of a buffer's file to kill ring"
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer))))
    (kill-new file-name)
    (message "Copied \"%s\"" file-name)))

(defun zk-orgwork-goto-latest-note-file ()
  "Go to the latest note org file under the same directory."
  (interactive)
  (let* ((file-list (directory-files default-directory nil "notes.*\\.org"))
         ;; directory-files sort the files alphabeticaly
         (latest-file (car (last file-list))))
    (unless latest-file (user-error "No notes file found"))
    (switch-to-buffer (find-file-noselect latest-file))))

(defun zk-orgwork-goto-orgwork-file ()
  "Go to the work.org file under the same directory if it exists."
  (interactive)
  (let ((file-name "work.org"))
    (unless (file-exists-p file-name) (user-error "%s does not exist" file-name))
    (switch-to-buffer (find-file-noselect file-name))))

(defun zk-org-generate-custom-id-from-text (text)
  "Generate a plain ID that only contains alphanumerics and
underscores from a natural text. Throw an error if the generated
CUSTOM_ID already exists in the file."
  (let ((new-id
         (downcase
          (replace-regexp-in-string
           "\\`_*" ""  ; remove leading underscores
           (replace-regexp-in-string
            "_*\\'" ""  ; remove trailing underscores
            (replace-regexp-in-string  ; replace non alphanumerics to underscores
             "[^a-zA-Z0-9_]+" "_" text))))))
    (if (zk-org-custom-id-exists-p new-id)
        (user-error "CUSTOM_ID \"%s\" already exists in the file. Try changing the headline to make it unique."
                    new-id))
    new-id))

(defun zk-org-custom-id-exists-p (custom-id)
  "Check if the given CUSTOM_ID already exists in the current org file."
  (require 'org)
  (let ((found-p nil))
    (org-map-entries (lambda ()
                       (if (string= custom-id
                               (org-element-property :CUSTOM_ID (org-element-at-point)))
                           (setq found-p t))))
    found-p))
  
(defun zk-org-set-generated-custom-id-and-copy-external-link ()
  "If CUSTOM_ID of the current org headline doesn't exist,
generate one based on the text of the headline and set it. This
will also move to that headline. Return the external link based
on the CUSTOM_ID."
  (interactive)
  (require 'org)
  (require 'org-element)
  (zk-org-move-to-current-heading)
  (let* ((headline (org-element-at-point))
         (custom-id (or
                     (org-element-property :CUSTOM_ID headline)
                     (let ((new-id
                            (zk-org-generate-custom-id-from-text
                             (substring-no-properties (org-get-heading t t t t)))))
                       (org-set-property "CUSTOM_ID" new-id)
                       new-id)))
         (link (concat "file:"
                       (file-name-nondirectory (buffer-file-name (current-buffer)))
                       "::#"
                       custom-id)))
    (kill-new link)
    (message "Copied \"%s\"" link)))

(defun zk-org-move-to-current-heading ()
  "Move to the current heading if not already at a heading."
  (interactive)
  (require 'org-element)
  (unless (eq 'headline (org-element-type (org-element-at-point)))
    (org-previous-visible-heading 1)))

(defun zk-org-tags-view (arg)
  "org-tags-view will always ask for the tags before switching to
an existing view buffer if available, but it doesn't use the
entered tags anyway if org-agenda-sticky is turned
on. zk-org-tags-view will try to switch to the existing buffer
without asking."
  (interactive "P")
  (let* ((view-buffer-name (if arg "*Org Agenda(M)*" "*Org Agenda(m)*"))
         (view-buffer (get-buffer view-buffer-name)))
    (if view-buffer
        (progn
          (display-buffer view-buffer)
          (select-window (get-buffer-window view-buffer)))
      (org-tags-view arg))))

(defun zk-org-set-tags-command ()
  "Set tags to the current entry. It's better than
org-set-tags-command in that it uses the agenda files instead of
the current file for completion."
  (interactive)
  (let* ((current-tags (org-get-tags nil t))
         (new-tag (completing-read
                   (concat "Tags: " (org-make-tag-string current-tags))
                   (org-global-tags-completion-table)
                   nil
                   nil
                   nil
                   t)))
    (org-toggle-tag new-tag 'on)))

(require 'dash)
(defun zk-org-rename-tag-command ()
  "Rename a tag throughout the agenda files."
  (interactive)
  ;; Save all files before the renaming, to give the user a chance to
  ;; discard the rename if they change their mind.
  (save-some-buffers)
  (let* ((all-tags (mapcar #'car (org-global-tags-completion-table)))
         (from-tag (completing-read
                    "Rename tag: "
                    all-tags
                    nil
                    t
                    nil
                    t))
         (to-tag (zk-trim-string
                  (read-string (format "Rename tag \"%s\" to (empty string to delete): " from-tag) from-tag t)))
         (counter 0))
    (when (cond ((string= from-tag to-tag)
                 (progn (message "The new tag is the same as the old tag") nil))
                ((string= "" to-tag)
                 (yes-or-no-p
                  (format "Do you want to remove tag \"%s\" from all entries?" from-tag)))
                ((-contains? all-tags to-tag)
                 (yes-or-no-p
                  (format "Do you want to merge \"%s\" into \"%s\"?" from-tag to-tag)))
                (t t))
      (org-map-entries
       (lambda ()
         (when (-contains? (org-get-tags nil t) from-tag)
           (org-toggle-tag from-tag 'off)
           (setq counter (+ 1 counter))
           (unless (string= "" to-tag)
             (org-toggle-tag to-tag 'on))))
       t
       'agenda-with-archives)
      (if (string= "" to-tag)
          (message "Removed \"%s\" in %d entries" from-tag counter)
        (message "Changed \"%s\" to \"%s\" in %d entries" from-tag to-tag counter)))))

(defun zk-days-between-dates (&rest dates)
  "Given a list of dates and calculate the days between
  them. Dates are in the format that org-read-date supports, such
  as \"YYYY-MM-DD\", \"+1\", \"today\""
  (require 'org)
  (when (> (length dates) 1)
    (let* ((first (car dates))
           (rest (cdr dates))
           (second (car rest)))
      (append (list (- (org-time-string-to-absolute (org-read-date nil nil second))
                       (org-time-string-to-absolute (org-read-date nil nil first))))
              (apply 'zk-days-between-dates rest)))))

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

(defun zk-open-file-path-from-region-or-at-point ()
  "If mark is active, open a file whose path is specified by the
region. Otherwise, open a file whose path is specified by the
file name at point."
  (interactive)
  (if mark-active
      (let ((path (buffer-substring-no-properties (mark) (point))))
        (if (> (length path) 0)
	    (switch-to-buffer (find-file-noselect path))
          (user-error "Selection is empty")))
    (let ((path (thing-at-point 'filename)))
      (if path
          (switch-to-buffer (find-file-noselect path))
        (user-error "No file name at point")))))

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

(defun zk-save-local-copy-if-remote-file ()
  "If the current buffer is a remote file, save a local copy for it"
  (interactive)
  (if (zk-file-remote-p (buffer-file-name))
      (zk-save-local-copy-for-remote-file)))

(defvar zk-remote-file-prefix-list (list "/google/data/rw")
  "The list of file prefixes to be used by
zk-save-local-copy-if-remote-file to decide whether a file is a
remote file.")

(defun zk-file-remote-p (path)
  (require 'dash)
  (or (file-remote-p path)
      (-contains? (mapcar #'(lambda (prefix)
                              (string-prefix-p prefix path))
                          zk-remote-file-prefix-list) t)))

(defun zk-save-local-copy-for-remote-file ()
  "Save a local copy for the current buffer as if it's a remote
file.  This can be used for saving an emergency backup when the
remote directory suddenly becomes inaccessible."
  (interactive)
  (let ((basedir "~/.emacs.d/remote-file-copies/"))
    (make-directory basedir t)
    (let ((localfile
           (concat basedir
                   (replace-regexp-in-string "/" "#" (buffer-file-name)))))
      (zk-save-buffer-as-copy localfile))))

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

(defun zk-clipboard-cut (arg)
  "Delete the current region (selection) and send it to clipboard
using desktop-helper. With prefix argument (C-u), remove line
breaks within paragraphs in the saved content."
  (interactive "P")
  (zk-clipboard-copy arg)
  (delete-region (region-beginning) (region-end)))

(defun zk-clipboard-copy (arg)
  "Save the current region (selection) to clipboard using
desktop-helper. With prefix argument (C-u), remove line breaks
within paragraphs in the saved content."
  (interactive "P")
  (unless mark-active
    (user-error "Region not active"))
  (let ((buffer (current-buffer))
        (begin (region-beginning))
        (end (region-end)))
    (if (= begin end)
        (user-error "No region selected"))
    (if arg (with-temp-buffer
              (insert-buffer-substring buffer begin end)
              (mark-whole-buffer)
              (zk-remove-line-breaks-within-paragraphs-region)
              (zk-clipboard-copy nil))
      (with-temp-buffer
        ;; Use a temp-buffer for client output
        (let ((temp-buffer (current-buffer)))
          (with-current-buffer buffer
            (call-process-region
             begin end
             "desktop-helper-client.py"
             nil temp-buffer nil
             "store-to-clipboard")))
        (message (zk-trim-string (buffer-string)))))
    (deactivate-mark)))

(defun zk-remove-line-breaks-within-paragraphs-region ()
  "Join all lines, except empty lines, within the region.  This
effectively removes all line breaks within paragraphs, making the
text suitable for copying to line-wraping text editors."
  (unless mark-active
    (user-error "Region is not active"))
  (let ((begin (region-beginning))
        (end (region-end)))
    (save-mark-and-excursion
      (goto-char begin)
      ;; Replace every new-line and its adjacent blanks with one space
      (while (search-forward-regexp
              "\\([[:graph:]]\\)[[:blank:]]*\n[[:blank:]]*\\([[:graph:]]\\)" end t)
        (replace-match "\\1 \\2")))))

(defun zk-clipboard-paste ()
  "Retrieve the clipboard from desktop-helper and insert to the current point."
  (interactive)
  ;; Use a temp file for client output that is not the content
  (let ((temp-file (make-temp-file "zk-clipboard-paste")))
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
    (zk-clipboard-paste)
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

(defvar zk-youdao-dict--history nil)
(defun zk-youdao-dict (word)
  "Look up the word in Youdao dictionary."
  (interactive (list
                (read-string "Youdao: "
                             nil
                             'zk-youdao-dict--history)))
  (eww-browse-url (concat "https://dict.youdao.com/w/eng/" (url-hexify-string word))) t)

(defun zk-clipboard-youdao-dict (word)
  "Look up the word in Youdao dictionary. The input is prefilled from clipboard."
  (interactive (list
                (read-string "Youdao: "
                             (zk-clipboard-get-string)
                             'zk-youdao-dict--history)))
  (zk-youdao-dict word))

(defun zk-switch-to-other-buffer ()
  "Switch to the other buffer without asking."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun zk-yank-to-register (r)
  "Yank to a register"
  (interactive (list (register-read-with-preview
                      (format "Yank \"%s\" to register: "
                              (let ((content (current-kill 0 t))
                                    (limit 80))
                                (replace-regexp-in-string
                                 "\n+" " "
                                 (if (> (length content) limit)
                                     (concat (substring content 0 limit) "...")
                                   content)))))))
  (set-register r (current-kill 0 t)))

;;; Advice compilation-find-file to replace "\" with "/" in file names
;;; if the system is cygwin.  javac under windows produces error
;;; messages where file names use "\" as separators. While emacs can
;;; still open it thanks to cygwin, but emacs treats the whole path as
;;; the file name because it doesn't recognize "\" as the path
;;; separator. compilation-find-file is the function compilation-mode
;;; uses to open a file from the error message. We filter its file
;;; name argument and fix its separators.
(defun zk-cygwin-filter-compilation-find-file-args (args)
  ;; Second argument is the file name
  (let* ((args-copy (copy-sequence args))
        (orig-file-name (nth 1 args))
        (new-file-name (replace-regexp-in-string "\\\\" "/" orig-file-name)))
    (setf (nth 1 args-copy) new-file-name)
    (message "zk-filter-compilation-find-file-args: changed file name \"%s\" to \"%s\""
             orig-file-name new-file-name)
    args-copy))

(defun zk-cygwin-fix-windows-path (orig-file-name)
  "Convert
  '/cygdrive/c/Windows/system32/\"D:\\Users\\zhangkun\\a.txt\\\"' (with
  windows path quoted) or
  '/cygdrive/c/Windows/system32/D:\\Users\\zhangkun\\a.txt\\' (with
  windows path not quoted) to
  '/cygdrive/d/Users/zhangkun/a.txt'. This is used to fix the
  file path when Windows uses Cygwin Emacs to open a file"
  (if (or (string-match "\"\\([A-Z]\\):\\\\\\([^\"]*\\)\"$" orig-file-name)
          (string-match "[^\"]\\([A-Z]\\):\\\\\\(.*\\)$" orig-file-name))
      (concat
         "/cygdrive/"
         (downcase (match-string 1 orig-file-name))  ; The drive letter
         "/"
         (replace-regexp-in-string "\\\\" "/" (decode-coding-string (match-string 2 orig-file-name) 'utf-8)))
      orig-file-name))

(defun zk-cygwin-advice-find-file-fix-windows-path (args)
  "An advice to fix the file path passed to find-file-noselect when
Windows uses Cygwin Emacs to open a file which invokes find-file-noselect"
  ;; First argument is the file name
  (let* ((args-copy (copy-sequence args))
        (orig-file-name (nth 0 args))
        (new-file-name (zk-cygwin-fix-windows-path orig-file-name)))
    (unless (string-equal orig-file-name new-file-name)
      (setf (nth 0 args-copy) new-file-name)
      (message "zk-cygwin-fix-windows-path changed file name \"%s\" to \"%s\""
	       orig-file-name new-file-name))
    args-copy))

(defun zk-cygwin-dired-cygstart ()
  "Run cygstart on selected files in dired."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (eq 0 (call-process "cygstart" nil nil nil file))
        (message "Successfully called cygstart for '%s'" file)
      (message "cygstart for '%s' failed" file))))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
              "[ \t\n]*$" "" (shell-command-to-string
                      "$SHELL --login -c 'echo $PATH'"
                            ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Cygwin-specific hacks
(when (eq system-type 'cygwin)
  (message "Cygwin detected, installing advices")
  (advice-add 'compilation-find-file :filter-args #'zk-cygwin-filter-compilation-find-file-args)
  (advice-add 'find-file-noselect :filter-args #'zk-cygwin-advice-find-file-fix-windows-path)
  ;; When Windows starts Cygwin Emacs, it's not from a login shell thus paths won't be set.
  ;; This will set exec-path and the PATH environment correctly.
  (unless (member "/bin" exec-path)
    (message "Explicitly setting exec-path and PATH for Cygwin")
    (set-exec-path-from-shell-PATH))
  ;; When Windows starts Cygwin Emacs, it's not from a shell thus the
  ;; SHELL environment is not set, causing shell-file-name to be set
  ;; to /bin/sh, overriding its default value /bin/bash, which caused
  ;; explicit-bash-args to be unused. Here I restore the value of
  ;; shell-file-name.
  (unless (getenv "SHELL")
    (message "SHELL environment is not set, forcing shell-file-name to bash")
    (setq shell-file-name "/bin/bash"))
  (add-hook 'dired-mode-hook
            (lambda ()
              (local-set-key (kbd "E") 'zk-cygwin-dired-cygstart))))

(when (string-prefix-p "/google/src/cloud" command-line-default-directory)
  (defun zk-google3-find-g4-opened-file(f)
    "Find an opened file in the g4 client"
    (interactive
     (list (completing-read
            "Find an g4 opened file: "
            (-map 'zk-project-get-relative-path
                  (process-lines
                   "bash" "-c"
                   (concat "cd " zk-project-root "; g4 whatsout"))))))
    (find-file (zk-project-restore-absolute-path f)))
  (global-set-key (kbd "C-z g f") 'zk-google3-find-g4-opened-file)

  (defun zk-google3-open-in-codesearch()
    "Open the current file in codesearch and focus on the current line."
    (interactive)
    (browse-url (concat "http://cs/piper///depot/google3/"
            (zk-project-get-relative-path (buffer-file-name))
            ";l="
            (number-to-string (line-number-at-pos)))))
  (global-set-key (kbd "C-z g s") 'zk-google3-open-in-codesearch)

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
  (global-set-key (kbd "C-z g p") 'zk-google3-open-build-sponge-link)

  (defun zk-google3-open-critique()
    "Open the critique page of the current file. If the current
buffer doesn't visit a file, let the user select a CL to open."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (if file-name
          (shell-command (concat "open-critique-for-file "
                                 (prin1-to-string (zk-project-get-relative-path file-name))))
        ;; If not visiting a file, let the user select a CL
        (let* ((selected-line
                (completing-read "Open Critique for: "
                                 (process-lines "bash" "-c" "g4 p | grep '^[* ]*Change [0-9]\\+' | sed 's/^[* ]*Change //'")))
               (selected-cl (if (string-match "\\(^[0-9]+\\) .*" selected-line)
                                (match-string 1 selected-line)
                              (user-error "No CL selected"))))
          (zk-browse-url (concat "http://cl/" selected-cl))))))
  (global-set-key (kbd "C-z g c") 'zk-google3-open-critique)

  (defun zk-google3-open-file-from-codesearch-or-critique-link (link)
    "Open a file indicated by the given codesearch or critique link"
    (interactive (list
                  (read-string "CodeSearch or Critique link: " (zk-clipboard-get-string))))
    (cond ((string-prefix-p "https://critique.corp.google.com" link)
           (string-match "https://[a-z.]*/cl/[0-9]+/depot/google3/\\([^;?]+\\)\\(.*\\)" link))
          ((string-prefix-p "https://source.corp.google.com" link)
           (string-match "https://[a-z.]*/piper///depot/google3/\\([^;?]+\\)\\(.*\\)" link))
          (t (user-error "Not a CodeSearch or Critique link")))
    (let* ((path (match-string 1 link))
           (params-substring (match-string 2 link))
           (line-string (cond ((string-match ";l=\\([0-9]+\\)" params-substring) (match-string 1 params-substring))
                              ((string-match "#\\([0-9]+\\)" params-substring) (match-string 1 params-substring))
                              (t nil)))
           (line (if line-string (string-to-number line-string))))
      (if path
          (progn
            (switch-to-buffer (find-file-noselect (zk-project-restore-absolute-path path)))
            (if line (goto-line line)))
        (user-error "Cannot parse the link"))))
  (global-set-key (kbd "C-z g M-f") 'zk-google3-open-file-from-codesearch-or-critique-link)

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
  (global-set-key (kbd "C-z g e") 'zk-google3-g4-edit)
)

(provide 'zk)
