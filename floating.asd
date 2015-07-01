(defsystem :floating
  :description "Print and parse floating-point numbers in any radix and with any precision. This is useful for persistent exact floats."
  :version "0.0.1"
  ;; :author
  ;; :license
  ;; the :depends-on loads the listed packages; these will be available when
  ;; compiling the :components, but not after the defsystem in this file,
  ;; i.e. (use-package 'alexandria) won't have the intended effect
  ;; neither (defpackage :utils (:use :alexandria))
  :depends-on ()
  :components ((:file "floating")))
