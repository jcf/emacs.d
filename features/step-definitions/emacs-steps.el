(Then "^the current major mode should be \"\\([^\"]+\\)\"$"
  (lambda (mode)
    (should (string= (symbol-name major-mode) mode))))
