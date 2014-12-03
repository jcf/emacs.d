Feature: Startup

  # This isn't what we want, but this is what we get when ecukes starts up
  # Emacs.
  Scenario: When starting up Emacs
    Then the cursor should be at point "1"
    And the current major mode should be "lisp-interaction-mode"
    And the buffer should be empty
