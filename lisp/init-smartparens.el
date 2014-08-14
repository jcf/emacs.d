;;; init-smartparens.el --- Loads and configures smartparens
;;; Commentary:
;;; https://github.com/Fuco1/smartparens
;;; Code:
(require-package 'smartparens)

;; I don't need paredit, but some package developers do!
(require-package 'paredit)

(after-load 'smartparens
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

(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;; Close a backtick with another backtick in clojure-mode
(sp-local-pair 'clojure-mode "`" "`" :when '(sp-in-string-p))

(sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))

(provide 'init-smartparens)
;;; init-smartparens ends here
