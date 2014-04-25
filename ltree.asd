;; LTREE (labelled tree) structure.

(defpackage :ltree-system
  (:use :common-lisp :asdf))

(in-package :ltree-system)

(defsystem :ltree
  :name "ltree"
  :author "Anton G. Moll"
  :version "0.1"
  :maintainer "Anton G. Moll"
  :description "LTREE (labelled tree) structure"
  :components ((:file "ltree")))
