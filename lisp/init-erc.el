;;; init-erc.el --- Sets up ERC
;;; Commentary:
;;; Code:

(after-load 'erc
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))

  (setq erc-timestamp-only-if-changed-flag nil)
  (setq erc-timestamp-format "[%H:%M] ")
  (setq erc-insert-timestamp-function 'erc-insert-timestamp-left)

  (setq erc-truncate-mode t)

  (add-hook 'window-configuration-change-hook
            (lambda ()
              (setq erc-fill-column (- (window-width) 2)))))

(provide 'init-erc)
;;; init-erc.el ends here
