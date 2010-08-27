(in-package :common-lisp-user)

(defpackage :utils
  ;; this :use will make the symbols available in the package :utils,
  ;; not :common-lisp-user !
  (:use #:common-lisp :alexandria :asdf)
  (:export
   ;; from utils.lisp
   #:repeat
   #:nflatten
   #:flatten-1
   #:has-key
   #:unique-list-p
   #:unique-list
   #:asetf
   #:aif
   #:subseqbut
   #:defanaphoric
   #:arplacd
   #:unroll-do
   #:do-unrollable
   #:timeit
   #:range
   #:string->number
   #:string->integer
   #:cmp
   #:at-least
   #:at-most
   #:within-p
   #:within
   #:binary-search
   #:compile-binary-search
   #:emit-compile-binary-search
   #:const-fun
   #:list-nth
   #:lists-nth
   #:gmapcar
   #:sreplace
   #:string-tolower
   #:string-toupper
   #:compose-1
   #:maptree
   #:lcompose
   #:mapc-array-major
   #:mapc-array
   #:map-array
   #:unique
   #:andf
   #:orf
   #:equal-array
   #:all
   #:all-if
   #:let-array-dims
   ))
