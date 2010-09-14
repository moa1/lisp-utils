(defsystem :utils
  :description "utils: utilities"
  :version "0.0.1"
  ;; :author
  ;; :license
  ;; the :depends-on loads the listed packages; these will be available when
  ;; compiling the :components, but not after the defsystem in this file,
  ;; i.e. (use-package 'alexandria) won't have the intended effect
  ;; neither (defpackage :utils (:use :alexandria))
  :depends-on (:alexandria :cl-ppcre)
  :components ((:file "package")
	       (:file "utils" :depends-on ("package"))
	       (:file "utils2" :depends-on ("package" "utils"))
	       (:file "regex-utils" :depends-on ("package" "utils2"))
	       (:file "rational-sin-cos" :depends-on ("package" "utils2"))
	       (:file "import" :depends-on ("package"
					    "utils2"
					    "regex-utils"
					    "rational-sin-cos"
					    ))))
