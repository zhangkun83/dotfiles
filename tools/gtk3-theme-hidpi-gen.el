;; This is actually not used for my GTK3 HiDPI solution, because
;; GDK_SCALE works better.  I still check in this code for further
;; reference.
(defun gtk3-theme-hidpi-gen()
    "Find size strings in the format like 10px and replace them
    with double value like 20px"
    (interactive)
    (re-search-forward "\\<[0-9]+px\\>")
    (backward-word)
    (let ((num (thing-at-point 'number)))
      (replace-match (number-to-string (* num 2)))))

(global-set-key [f2] 'gtk3-theme-hidpi-gen)
