(defsystem :dlist2
  :name "dlist2"
  :author "Anton G. Moll"
  :version "0.1"
  :maintainer "Anton G. Moll"
  :description "Re-implementation of package dlist by Krzysztof Drewniak.
Both package dlist and dlist2 implement a doubly linked list data structure,
the main difference being the internal representation of DLISTs, which have
caps made up of DCONSes at the start and the end. This means that empty
dlists are two dconses pointing toward each other.
Some programs become faster (for example pushing onto and popping off a dlist),
some slower (for example creating a lot of small dlists)."
  :depends-on (:alexandria)
  :components ((:file "dlist2")))
