(defconst zk-user-home-dir
  (if (eq system-type 'windows-nt)
      (getenv "USERPROFILE")
    (getenv "HOME"))
  "The home directory on an OS where a users files are located.
On linux it's the same as $HOME. On Windows it's $USERPFOFILE,
whose value is \"C:\\Users\\foo\" and more preferrable
than $HOME which is \"c:\\Users\\foo\\AppData\\Roaming\".")

(defun zk-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

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

(defun zk-move-to-next-char (char)
  "Move point to where the char appears next"
  (if (search-forward (char-to-string char) nil t)
      ;; search-forward stops after the char. Move the point to at the char
      (backward-char)))

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

(defun zk-copy-buffer-file-path ()
  "Copy the full path of a buffer's file to kill ring"
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer))))
    (kill-new file-name)
    (message "Copied \"%s\"" file-name)))

(defun zk-sum-all-duration-marks-as-seconds ()
  "Find duration marks in the format of H:MM:SS as its own line in
the current buffer, sum them up and return the total seconds"
  (save-excursion
    (goto-char (point-min))
    (let ((total-seconds 0))
      (while (re-search-forward "^ *\\([0-9]+\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\) *$" nil t)
        (let ((hours (match-string 1))
              (minutes (match-string 2))
              (seconds (match-string 3)))
          (setq total-seconds
                (+ total-seconds
                   (string-to-number seconds)
                   (* 60 (string-to-number minutes))
                   (* 3600 (string-to-number hours))))))
      total-seconds)))

(defun zk-convert-seconds-to-hms (seconds)
  "Convert seconds to the format H:MM:SS"
  (let* ((hours (/ seconds 3600))
         (seconds (mod seconds 3600))
         (minutes (/ seconds 60))
         (seconds (mod seconds 60)))
    (format "%d:%02d:%02d" hours minutes seconds)))

(defun zk-sum-all-duration-marks ()
  "Find duration marks in the format of H:MM:SS as its own line in
the current buffer, sum them up and return the sum in the same
format.  This is useful for preparing presentations where the
duration of each slide is recorded with org-timer."
  (interactive)
  (let* ((total-seconds (zk-sum-all-duration-marks-as-seconds))
         (hms (zk-convert-seconds-to-hms total-seconds)))
    (message "Total duration: %s" hms)
    hms))

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

(defvar zk-frame-title-base-name "Emacs")

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

(defun zk-recreate-buffer (name)
  "Delete the buffer with the given name if it exists, and create one with same name.
   Return the new buffer.  This is preferred to just get-buffer-create because when
   a buffer is reused by the latter compilation-minor-mode stops recognizing error
   lines."
  (ignore-errors (kill-buffer name))
  (get-buffer-create name))

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
  (let ((program (cond ((eq system-type 'windows-nt)
                        "\"C:/Program Files/Google/Chrome/Application/chrome.exe\"")
                       ((eq system-type 'darwin)
                        "open")
                       (t "desktop-helper-client.py open-url"))))
    (shell-command (concat program " "
                         (prin1-to-string url)))))

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

;; Use relative directory for local paths, because I may be running
;; this script under MINGW64 (provided by Git bash) whose home is like
;; "/c/Users/zhangkun" while the rsync on the system is a Cygwin
;; version whose home is like "/cygdrive/c/home/zhangkun". Passing the
;; absolute path from this script to rsync won't work.
(defconst zk-syncbox-local-dirname "syncbox")
(defconst zk-syncbox-remote-dir "rsync://localhost:1873/syncbox")
(defconst zk-syncbox-buffer-name "*syncbox*")

(defun zk-syncbox (msg src dest)
  (switch-to-buffer zk-syncbox-buffer-name)
  (read-only-mode -1)
  (erase-buffer)
  (insert msg "\n")
  (let ((default-directory zk-user-home-dir))
    (call-process
     "rsync" nil zk-syncbox-buffer-name t 
     "-rtuv" (concat src "/") dest))
  (read-only-mode 1)
  (read-string "Press Enter to continue ...")
  (kill-buffer))

(defun zk-syncbox-download ()
  "Download from remote syncbox to local syncbox."
  (interactive)
  (zk-syncbox "Downloading from remote syncbox ..."
              zk-syncbox-remote-dir zk-syncbox-local-dirname))

(defun zk-syncbox-upload ()
  "Upload local syncbox to remote syncbox."
  (interactive)
  (zk-syncbox "Uploading to remote syncbox ..."
              zk-syncbox-local-dirname zk-syncbox-remote-dir))

(defun zk-copy-region-to-temp-buffer ()
  "Copy the content of the active region to a temporary buffer."
  (interactive)
    (unless mark-active
      (user-error "Region not active"))
    (let ((content (buffer-substring-no-properties (region-beginning) (region-end))))
      (switch-to-buffer (make-temp-name "*clip*"))
      (insert content)))

(defun zk-remove-line-breaks-within-paragraphs (arg)
  "Join all lines, except empty lines, in the active region.  This
effectively removes all line breaks within paragraphs, making the
text suitable for copying to line-wraping text editors. With the prefix
argument, apply the change to the entire buffer."
  (interactive "P")
  (unless (or arg mark-active)
    (user-error "Region is not active"))
  (let ((begin (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-mark-and-excursion
      (goto-char begin)
      ;; Replace every new-line and its adjacent blanks with one space
      (while (search-forward-regexp
              "\\([[:graph:]]\\)[[:blank:]]*\n[[:blank:]]*\\([[:graph:]]+\\)" end t)
        (let ((second-line (match-string 2)))
          ;; If the second line starts with a bullet prefix ("-", "*",
          ;; "+", or "1." "2." etc), it is meant to be a new line, so
          ;; don't join the two lines. This is common in markdown
          ;; texts.
          (unless (save-match-data (string-match "^\\([-*+]\\)\\|\\([0-9]+[.]\\)" second-line))
            (replace-match "\\1 \\2")))))))

(defun zk-popup-window-to-new-frame ()
  "If there is more than one windows, display the buffer of the
current window in a new frame and close that window"
  (interactive)
  (if (> (length (window-list nil 'no-minibuf)) 1)
      (let ((buffer (current-buffer)))
        (when (minibufferp buffer)
          (user-error "Current window is minibuffer"))
        (delete-window)
        (display-buffer-other-frame buffer))
    (user-error "Should have at least 2 windows")))

(defun zk-get-base-buffer (buffer)
  "Get the base buffer of the given buffer, if it's an indirect
buffer.  Otherwise, return the buffer itself."
  (or (buffer-base-buffer buffer) buffer))

(defun zk-goto-base-buffer ()
  "Go to the base buffer if the current buffer is an indirect
buffer.  Move the point to the current point of the indirect
 buffer." 
  (interactive)
  (let ((base-buffer (buffer-base-buffer))
        (pos (point)))
    (unless base-buffer
      (user-error "Not an indirect buffer"))
    (switch-to-buffer base-buffer)
    (goto-char pos)))

(defun zk-generate-frame-title ()
  (let ((retval zk-frame-title-base-name))
    ;; If this session has more than one frames, add the current
    ;; buffer name to the frame title
    (when (> (length (frame-list)) 1)
      (setq retval (concat (buffer-name (window-buffer)) " @" retval)))
    (setf (frame-parameter nil 'name) retval)
    retval))

(setq frame-title-format '(:eval (zk-generate-frame-title)))

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

(defun zk-dired-open-file-with-os ()
  "Use OS-specific command to open a selected file in dired."
  (interactive)
  (let ((file (dired-get-file-for-visit))
        (cmd (cond ((eq system-type 'cygwin) "cygstart")
                   ((eq system-type 'windows-nt) "cmd")
                   ((eq system-type 'darwin) "open")
                   (t (user-error "Unsupported system: %s" system-type)))))
    ;; Use 'cmd /C "" <file-name>' to open on native Windows.
    ;; TODO: it doesn't work with unicode file names
    (if (eq 0 (cond ((eq system-type 'windows-nt) (call-process cmd nil nil nil "/C" "start" "" file))
                    (t (call-process cmd nil nil nil file))))
        (message "Successfully opened '%s' in '%s'" file system-type)
      (message "'%s' failed to open '%s'" system-type file))))

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key (kbd "E") 'zk-dired-open-file-with-os)))

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
    (setq shell-file-name "/bin/bash")))

(provide 'zk)
