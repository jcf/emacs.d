(require-package 'sql-indent)
(after-load 'sql
  (require 'sql-indent))

(defun jcf/pop-to-sqli-buffer ()
  "Switch to the corresponding sqli buffer."
  (interactive)
  (if sql-buffer
      (progn
        (pop-to-buffer sql-buffer)
        (goto-char (point-max)))
    (sql-set-sqli-buffer)
    (when sql-buffer
      (jcf/pop-to-sqli-buffer))))

(after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-z") 'jcf/pop-to-sqli-buffer)
  (add-hook 'sql-interactive-mode-hook 'jcf/never-indent)
  (when (package-installed-p 'dash-at-point)
    (defun jcf/maybe-set-dash-db-docset ()
      (when (eq sql-product 'postgres)
        (setq dash-at-point-docset "psql")))

    (add-hook 'sql-mode-hook 'jcf/maybe-set-dash-db-docset)
    (add-hook 'sql-interactive-mode-hook 'jcf/maybe-set-dash-db-docset)
    (defadvice sql-set-product (after set-dash-docset activate)
      (jcf/maybe-set-dash-db-docset))))

(setq-default sql-input-ring-file-name
              (expand-file-name ".sqli_history" user-emacs-directory))

(after-load 'page-break-lines
  (push 'sql-mode page-break-lines-modes))

(provide 'init-sql)
