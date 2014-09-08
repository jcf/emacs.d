
;; [[file:~/.emacs.d/README.org::*Benchmarks][Benchmarks:1]]

(defun jcf/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar jcf/require-times nil
  "A list of (FEATURE . LOAD-DURATION). LOAD-DURATION is the time
  taken in milliseconds to load FEATURE.")

(defadvice require
  (around build-require-times (feature &optional filename noerror) activate)
  "Note in `jcf/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (add-to-list 'jcf/require-times
                     (cons feature
                           (jcf/time-subtract-millis (current-time)
                                                           require-start-time))
                     t)))))

;; Benchmarks:1 ends here

;; [[file:~/.emacs.d/README.org::*init.el][init\.el:1]]

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; init\.el:1 ends here

;; [[file:~/.emacs.d/README.org::*cl][cl:1]]

(require 'cl)

;; cl:1 ends here

;; [[file:~/.emacs.d/README.org::*OS%20X][OS\ X:1]]

(defconst *is-a-mac* (eq system-type 'darwin))

;; OS\ X:1 ends here

;; [[file:~/.emacs.d/README.org::*Load%20Org%20files][Load\ Org\ files:1]]

(require 'org-install)
(require 'ob-tangle)

(defvar jcf/config-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

(defun jcf/load-org (s)
  (org-babel-load-file
   (expand-file-name (format "init-%s.org" s) jcf/config-dir)))

(add-hook
 'after-init-hook
 (lambda ()
   (jcf/load-org "defuns")
   (jcf/load-org "ubiquitous")
   (jcf/load-org "genesis")
   (jcf/load-org "presentation")
   (jcf/load-org "evil")
   (jcf/load-org "helm")
   (when *is-a-mac*
     (jcf/load-org "osx"))
   (jcf/load-org "org")
   (jcf/load-org "packages")
   (jcf/load-org "window-management")
   (jcf/load-org "version-control")
   (jcf/load-org "languages")
   (jcf/load-org "locales")))

;; Load\ Org\ files:1 ends here
