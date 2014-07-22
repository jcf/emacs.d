;;; init-company.el -- Installs and configures completion
;;; Commentary:
;;; Code:
(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

(defvar jcf/completion-backends
  '(company-go company-inf-ruby company-tern))

(dolist (package jcf/completion-backends)
  (require-package package))

(after-load 'company
  (diminish 'company-mode)

  ;; Add completion backends
  (dolist (backend jcf/completion-backends)
    (add-to-list 'company-backends backend)))

(provide 'init-company)
;;; init-company.el ends here
