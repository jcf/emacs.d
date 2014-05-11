;;; init-shell.el --- Shell script improved
;;; Commentary:
;;; Code:

(add-auto-mode 'sh-mode "\\.zsh\\'")

;; Use sh-mode for Prezto files
(defvar jcf-pretzo-files '("zlogin" "zlogin" "zlogout" "zpretzorc" "zprofile" "zshenv" "zshrc"))

(mapc (lambda (file)
        (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" file) . sh-mode)))
      jcf-pretzo-files)

(add-hook 'sh-mode-hook
          (lambda ()
            (if (and buffer-file-name
                     (member (file-name-nondirectory buffer-file-name) jcf-pretzo-files))
                (sh-set-shell "zsh"))))

(provide 'init-shell)
;;; init-shell.el ends here
