;;; init-markdown.el --- Adds Markdown support
;;; Commentary:
;;; Code:
(require-package 'markdown-mode)

(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)\\'" . markdown-mode) auto-mode-alist))

(require-package 'pandoc-mode)

(after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'turn-on-pandoc)
  (add-hook 'markdown-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "C-c /"))))

(provide 'init-markdown)
;;; init-markdown.el ends here
