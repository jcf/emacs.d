;;; init -- Configures Emacs
;;; Commentary:
;;; Code:
(require 'ob-tangle)
(org-babel-load-file
 (expand-file-name "README.org" user-emacs-directory))
;;; init.el ends here
