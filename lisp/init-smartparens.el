;;; init-smartparens.el --- Loads and configures smartparens
;;; Commentary:
;;; https://github.com/Fuco1/smartparens
;;; Code:
(require-package 'smartparens)

(after-load 'smartparens
  ;; We don't need paredit too
  (require 'paredit)
  (disable-paredit-mode)

  (setq smartparens-strict-mode t)
  (setq sp-autoinsert-if-followed-by-word t)

  (global-set-key (kbd "C-M-k") 'sp-kill-sexp-with-a-twist-of-lime)
  (global-set-key (kbd "C-M-f") 'sp-forward-sexp)
  (global-set-key (kbd "C-M-b") 'sp-backward-sexp)
  (global-set-key (kbd "C-M-n") 'sp-up-sexp)
  (global-set-key (kbd "C-M-d") 'sp-down-sexp)
  (global-set-key (kbd "C-M-u") 'sp-backward-up-sexp)
  (global-set-key (kbd "C-M-p") 'sp-backward-down-sexp)
  (global-set-key (kbd "C-M-w") 'sp-copy-sexp)
  (global-set-key (kbd "M-s")   'sp-splice-sexp)
  (global-set-key (kbd "M-r")   'sp-splice-sexp-killing-around)
  (global-set-key (kbd "C-)")   'sp-forward-slurp-sexp)
  (global-set-key (kbd "C-}")   'sp-forward-barf-sexp)
  (global-set-key (kbd "C-(")   'sp-backward-slurp-sexp)
  (global-set-key (kbd "C-{")   'sp-backward-barf-sexp)
  (global-set-key (kbd "M-S")   'sp-split-sexp)
  (global-set-key (kbd "M-J")   'sp-join-sexp)
  (global-set-key (kbd "C-M-t") 'sp-transpose-sexp)

  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  (sp-local-tag '(sgml-mode html-mode rhtml-mode)
                "<" "<_>" "</_>"
                :transform 'sp-match-sgml-tags))

;; Enable smartparens everywhere
(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

(provide 'init-smartparens)
;;; init-smartparens ends here
