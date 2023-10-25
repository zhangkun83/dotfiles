;; Utilities working with Microsoft Windows display scaling for HiDPI
;; displays.

;; As of Emacs 28.1, when running on Windows 10 with HiDPI displays,
;; Emacs will scale correctly at start up, but won't respond to
;; display scaling changes, e.g., when moving the Emacs frame to a
;; different monitor with a different scale factor (as set in Display
;; settings -> Scale and layout -> Change the size of text, apps and
;; other items.  Windows will try to scale it as an image to keep the
;; original window and font size, which will cause blurring.

;; IMPORTANT: this file works as expected only if Emacs is configured
;; such that "Properties -> Compatibility -> Change high DPI settings
;; -> Override high DPI scaling behavior. Scaling performed by:
;; Application" is selected.  With such setting, Windows will *not*
;; try to scale Emacs as an image when scaling factor changes, and
;; this file provides utilities to scale up or down the default font
;; to keep the original sizes

(require 'zk)

(defconst zk-mswin-scaling-monitor-scale-factors
  '(
    ;; Key is "(display-pixel-width):(display-mm-width)", value is a
    ;; list: (scale-factor name).  Where the scale factor is set in
    ;; "Display Settings".
    ("3840:697" . (225 "ThinkVision 32\""))
    ("3840:344" . (300 "Thinkpad P1G4 16\""))
    ("1920:532" . (125 "Acer 1080p 24\""))
    ))

(defun zk-mswin-scaling-get-current-monitor-scale-factor ()
  "Returns the scale factor of the current monitor, as a list of
the factor value, and the name of the monitor.  nil if not
found."
  (let* ((key (format "%d:%d" (display-pixel-width) (display-mm-width)))
         (value (alist-get key zk-mswin-scaling-monitor-scale-factors nil nil 'equal)))
    value))

(defun zk-mswin-scaling-format-scale-factor (value)
  "Format the return value of
zk-mswin-scaling-get-current-monitor-scale-factor."
  (format "`%s' at scale factor %d"
          (nth 1 value) (car value)))

(defconst zk-mswin-scaling-initial-scale-factor
  (let ((value (zk-mswin-scaling-get-current-monitor-scale-factor)))
    (message "zk-mswin-scaling: initial monitor: %s"
             (zk-mswin-scaling-format-scale-factor value))
    value))

(defun zk-mswin-scaling-scale-default-font-for-monitor ()
  "Scale the default font according to the current monitor scale factor,
so that the font appears the same size as was in the initial
scale factor.
"
  (interactive)
  (let* ((scale-factor (or (zk-mswin-scaling-get-current-monitor-scale-factor)
                           (list (read-number "Current monitor not recognized.  Enter scale factor: " 100)
                                 "User entered scale factor")))
         (font-size-factor
          (/ (float (car scale-factor))
             (car zk-mswin-scaling-initial-scale-factor))))
    (message "zk-mswin-scaling: initial monitor: %s, current monitor: %s"
             (zk-mswin-scaling-format-scale-factor zk-mswin-scaling-initial-scale-factor)
             (zk-mswin-scaling-format-scale-factor scale-factor))
    (zk-set-default-font zk-font-family font-size-factor)))

(provide 'zk-mswin-scaling)
