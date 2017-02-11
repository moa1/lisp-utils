(defsystem :mru-cache
  :name "mru-cache"
  :author "Anton G. Moll"
  :version "0.1"
  :maintainer "Anton G. Moll"
  :description "A most-recently-used cache.
Normal usage is to create a MRU cache using #'MAKE, add key-value-pairs using #'(SETF GET), and get elements using #'GET."
  :depends-on (:cl-custom-hash-table :dlist2)
  :components ((:file "mru-cache")))
