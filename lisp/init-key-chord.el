(require-package 'key-chord)

(setq key-chord-two-keys-delay 0.05)

(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

(provide 'init-key-chord)
