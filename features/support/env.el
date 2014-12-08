(require 's)
(require 'ert)
(require 'espuds)

(Setup
 (load (f-expand "init" user-emacs-directory) nil t))

(defun save-all-buffers-dont-ask ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (let ((filename (buffer-file-name)))
        (when (and filename
                   (or (file-exists-p filename)
                       (s-ends-with? ".clj" filename)))
          (save-buffer))))))

(defun kill-matching-buffers-dont-ask (regexp &optional internal-too)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

(After
 (save-all-buffers-dont-ask)
 (kill-matching-buffers-dont-ask ".*"))
