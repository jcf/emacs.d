;;; init-go.el --- Improved support for Go
;;; Commentary:
;;; Code:
(require-package 'go-mode)

;; It is assumed your GOPATH, and PATH are already setup. If this is
;; not the case you can set the relevant environment variables using
;; something like:
;;
;;   (setenv "GOPATH" (expand-file-name "Go" (getenv "HOME")))
;;   (setenv "PATH" (concat (getenv "PATH") ":"
;;                          (expand-file-name "bin" (getenv "GOPATH"))))

;; Require goflymake if it's installed.
;;
;; This dependency is managed outside of Emacs. You can install
;; goflymake like so:
;;
;;   go get -u github.com/dougm/goflymake
(defvar go-flymake-path
  (expand-file-name "src/github.com/dougm/goflymake" (getenv "GOPATH")))

(defun jcf-setup-go-flymake ()
  "Add PATH to 'load-path', and require go-flymake."
  (add-to-list 'load-path go-flymake-path)
  (require 'go-flymake))

(if (file-exists-p go-flymake-path) (jcf-setup-go-flymake))

;; Format our code automatically when saving
(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'init-go)
;;; init-go.el ends here
