;;; init-snippets.el -- Sets up tab-triggered snippets
;;; Commentary:
;;; Code:
(require-package 'yasnippet)
(require-package 'string-utils)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(provide 'init-snippets)
;;; init-snippets.el ends here
