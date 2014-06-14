;;; init-eshell.el --- Setup eshell
;;; Commentary:
;;; Code:

(setq eshell-scroll-to-bottom-on-input 'all)
(setq eshell-buffer-shorthand t)

(setq eshell-cmpl-ignore-case t)
(setq eshell-error-if-no-glob t)
(setq eshell-glob-case-insensitive t)

(defun eshell/clear ()
  "Clears all history within the current eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; Custom eshell prompt
(defun jcf/eshell-prompt ()
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
          " "))

(setq eshell-prompt-function 'jcf/eshell-prompt)

(provide 'init-eshell)
;;; init-eshell.el ends here
