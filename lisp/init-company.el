;;; init-company.el -- Installs and configures completion
;;; Commentary:
;;; Code:
(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require-package 'company-go)
(require-package 'company-inf-ruby)
(require-package 'company-tern)

;; Add completion backends
(after-load 'company
  (diminish 'company-mode)
  (add-to-list 'company-backends 'company-go)
  (add-to-list 'company-backends 'company-inf-ruby)
  (add-to-list 'company-backends 'company-tern))

(provide 'init-company)
;;; init-company.el ends here
