(defsystem :walker
  :description "A syntactic parser of Common Lisp.
Its scope is the syntactic handling of arbitrary Common Lisp code, i.e. parsing a list into an abstract syntax tree, and TODO: FIXME: converting an abstract syntax tree into a list.
It should have as little semantics in it as possible (while still being useful as a Common Lisp parser). In particular, there is no notion of constant variables (so NIL and T are ordinary variable names).
Type declarations are parsed, but the contained types are neither parsed nor interpreted."
  :version "0.0.1"
  ;; :author
  ;; :license
  :depends-on ()
  :components ((:file "walker")))
