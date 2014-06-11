;;; Character sets

(defcustom jcf/force-default-font-for-symbols nil
  "When non-nil, force Emacs to use your default font for symbols."
  :type 'boolean)

(defun jcf/maybe-use-default-font-for-symbols ()
  "Force Emacs to render symbols using the default font, if so configured."
  (when jcf/force-default-font-for-symbols
    (set-fontset-font "fontset-default" 'symbol (face-attribute 'default :family))))

(add-hook 'after-init-hook 'jcf/maybe-use-default-font-for-symbols)


;;; Changing font sizes

(require 'cl)

(defun jcf/font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun jcf/increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
Emacs will keep the pixel size of the frame approximately the
same.  DELTA should be a multiple of 10, to match the units used
by the :height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        ;; Latest 'set-frame-font supports a "frames" arg, but
        ;; we cater to Emacs 23 by looping instead.
        (set-frame-font (jcf/font-name-replace-size
                         (face-font 'default)
                         new-point-height)
                        t)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun jcf/increase-default-font-height ()
  (interactive)
  (jcf/increment-default-font-height 10))

(defun jcf/decrease-default-font-height ()
  (interactive)
  (jcf/increment-default-font-height -10))

(global-set-key (kbd "C-M-=") 'jcf/increase-default-font-height)
(global-set-key (kbd "C-M--") 'jcf/decrease-default-font-height)

(if *is-a-mac*
    (set-frame-font "-*-Source Code Pro for Powerline-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))

(provide 'init-fonts)
