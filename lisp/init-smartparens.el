;;; init-smartparens.el --- Loads and configures smartparens
;;; Commentary:
;;; https://github.com/Fuco1/smartparens
;;; Code:
(require-package 'smartparens)

(after-load 'smartparens
  ;; We don't need paredit too
  (require 'paredit)
  (disable-paredit-mode))

;; Enable smartparens everywhere
(require 'smartparens-config)

(setq smartparens-strict-mode t)
(setq sp-autoinsert-if-followed-by-word t)
(setq sp-autoskip-closing-pair 'always)
(setq sp-base-key-bindings 'paredit)
(setq sp-hybrid-kill-entire-symbol nil)

(smartparens-global-mode 1)
(show-smartparens-global-mode +1)

(sp-use-paredit-bindings)

(sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
(sp-local-tag '(sgml-mode html-mode rhtml-mode)
              "<" "<_>" "</_>"
              :transform 'sp-match-sgml-tags)

(provide 'init-smartparens)
;;; init-smartparens ends here
