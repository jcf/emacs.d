(require 'f)

(defvar jcf-support-path
  (f-dirname load-file-name))

(defvar jcf-features-path
  (f-parent jcf-support-path))

(defvar jcf-root-path
  (f-parent jcf-features-path))

(add-to-list 'load-path jcf-root-path)

(require 'jcf)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
