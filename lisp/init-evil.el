;;; init-evil.el --- Sets up Evil
;;; Commentary:
;;; Loads up Evil, activates it, and configures it with custom leading
;;; bindings, and more.
;;; Code:

(require-package 'evil)
(require-package 'evil-exchange)
(require-package 'evil-indent-textobject)
(require-package 'evil-leader)
(require-package 'evil-matchit)
(require-package 'evil-nerd-commenter)
(require-package 'evil-numbers)
(require-package 'evil-visualstar)
(require-package 'goto-chg)
(require-package 'evil-surround)

(setq evil-default-cursor t)
(setq evil-insert-state-message nil)
(setq evil-visual-state-message nil)
(setq evil-mode-line-format 'before)

(setq evil-search-module 'evil-search)

(setq evil-emacs-state-cursor  '("red" box))
(setq evil-normal-state-cursor '("gray" box))
(setq evil-visual-state-cursor '("gray" box))
(setq evil-insert-state-cursor '("gray" bar))
(setq evil-motion-state-cursor '("gray" box))

;; Activate evil-mode after global-evil-leader-mode (http://j.mp/1i0vLSP)
(global-evil-leader-mode)
(evil-mode 1)
(global-evil-surround-mode 1)
(global-evil-matchit-mode 1)
(evil-exchange-install)

(define-key evil-motion-state-map (kbd "C-]") 'ggtags-find-tag-dwim)

(define-key evil-normal-state-map (kbd "C-A")
  'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-A")
  'evil-numbers/dec-at-pt)

(define-key evil-normal-state-map "Y" (kbd "y$"))
(define-key evil-normal-state-map (kbd "SPC") 'evil-repeat-find-char)
(define-key
  evil-normal-state-map (kbd "S-SPC") 'evil-repeat-find-char-reverse)

(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-visual-state-map ";" 'evil-ex)

;; C-hjkl to move around windows
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; Magit from avsej
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)

(defadvice evil-search-next
    (after advice-for-evil-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-search-previous
    (after advice-for-evil-search-previous activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

;; Setup initial Evil mode for a number of commonly used modes.
(loop for (mode . state)
      in '((ielm-mode . insert)
           (nrepl-mode . insert)
           (shell-mode . insert)
           (git-rebase-mode . emacs)
           (term-mode . emacs)
           (help-mode . emacs)
           (helm-grep-mode . emacs)
           (grep-mode . emacs)
           (bc-menu-mode . emacs)
           (magit-branch-manager-mode . emacs)
           (rdictcc-buffer-mode . emacs)
           (dired-mode . normal)
           (wdired-mode . normal))
      do (evil-set-initial-state mode state))

;; Use escape to quit, and not as a meta-key.
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(defun jcf/delete-window ()
  "Delete the current window, and rebalance remaining windows."
  (interactive)
  (delete-window)
  (balance-windows))

(defun jcf/split-window-horizontally ()
  "Create a new horizontal split and rebalance windows."
  (interactive)
  (split-window-horizontally)
  (balance-windows))

(defun jcf/split-window-vertically ()
  "Create a new vertical split and rebalance windows."
  (interactive)
  (split-window-vertically)
  (balance-windows))

(evil-leader/set-leader ",")

(evil-leader/set-key
  "a"  'projectile-toggle-between-implementation-and-test
  "b"  'ibuffer
  "db" 'kill-buffer
  "dw" 'jcf/delete-window
  "eb" 'eval-buffer
  "ed" 'eval-defun
  "ee" 'eval-expression
  "es" 'eval-last-sexp
  "er" 'eval-region
  "fb" 'ido-switch-buffer
  "fd" 'ido-dired
  "ff" 'ido-find-file
  "gb" 'magit-blame-mode
  "gc" 'magit-commit
  "gl" 'magit-log
  "gs" 'magit-status
  "hf" 'describe-function
  "hm" 'describe-mode
  "hp" 'describe-package
  "hv" 'describe-variable
  "pb" 'projectile-switch-to-buffer
  "pd" 'projectile-switch-project
  "pf" 'projectile-find-file
  "sd" 'jcf/delete-window
  "sf" 'delete-other-windows
  "ss" 'jcf/split-window-horizontally
  "sv" 'jcf/split-window-vertically
  "w"  'save-buffer
  "x"  'smex)

(evil-leader/set-key-for-mode 'org-mode
  "t" 'org-todo
  "s" 'org-sort-list)

(evil-leader/set-key-for-mode 'clojure-mode
  "v" 'clojure-test-run-test
  "V" 'clojure-test-run-tests
  "cc" 'cider-connect
  "cj" 'cider-jack-in
  "cq" 'cider-quit
  "rs" 'cljr-sort-ns
  "rr" 'cljr-add-require-to-ns
  "ru" 'cljr-add-use-to-ns)

(evil-leader/set-key-for-mode 'ruby-mode
  "v" 'rspec-verify
  "V" 'rspec-verify-all)

(evil-leader/set-key-for-mode 'feature-mode
  "v" 'feature-verify-scenario-at-pos
  "V" 'feature-verify-all-scenarios-in-buffer)

;; C-n and C-p to select completions
(after-load 'auto-complete
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous))

(provide 'init-evil)
;;; init-evil.el ends here
