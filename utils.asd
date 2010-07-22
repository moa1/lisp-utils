(defpackage utils
  (:use :common-lisp :asdf))

(defsystem "utils"
  :description "utils: utilities"
  :version "0.0.1"
  ;; :author
  ;; :license
  :components ((:file "utils")
	       (:file "regex-utils" :depends-on ("utils"))))
