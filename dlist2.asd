;; Doubly linked list data structure

(defpackage :dlist2-system
  (:use :common-lisp :asdf))

(in-package :dlist2-system)

(defsystem :dlist2
  :name "dlist2"
  :author "Anton G. Moll"
  :version "0.1"
  :maintainer "Anton G. Moll"
  :description "Re-implementation of package dlist by Krzysztof Drewniak"
  :depends-on (:alexandria)
  :components ((:file "dlist2")))
