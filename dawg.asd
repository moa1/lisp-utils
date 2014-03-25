;; DAWG (directed acyclic word graph) structure.

(defpackage :dawg-system
  (:use :common-lisp :asdf))

(in-package :dawg-system)

(defsystem :dawg
  :name "dawg"
  :author "Anton G. Moll"
  :version "0.1"
  :maintainer "Anton G. Moll"
  :description "Directed acyclic word graph (DAWG) structure"
  :components ((:file "dawg")))
