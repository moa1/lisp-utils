(in-package :common-lisp-user)

(defpackage :utils
  ;; this :use will make the symbols available in the package :utils,
  ;; not :common-lisp-user !
  (:use #:common-lisp #:alexandria)
  (:export
   ;; from utils.lisp
   #:repeat
   #:nflatten
   #:flatten-1
   #:has-key
   #:unique-list-p
   #:unique-list
   #:it ;; for anaphoretic macros
   #:asetf
   #:aif
   #:subseqbut
   #:defanaphoric
   #:arplacd
   #:avalues
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
   #:binary-search-emitters
   #:search-elt ;; for binary-search-emitters
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
   #:andf
   #:orf
   #:equal-array
   #:all
   #:all-if
   #:let-array-dims
   #:vector->list
   #:head
   #:extract-keywords
   #:specializing-flet
   #:specializing-labels
   #:specializing-defun
   #:mappair
   #:mappair*
   #:mapl-maxn
   #:mapl*
   #:progn-repeat
   #:prind
   #:sgn
   #:2nd-value
   #:let+
   #:d-b-lambda
   #:log2
   #:pow2
   #:filter
   #:filter-if
   #:join
   #:member-tree
   #:exp-until-predicate
   #:timecps
   #:specializing-cases
   #:specializing-cond-let
   #:unique
   #:dump-to-file
   #:no-error
   #:timediff
   #:length>=
   #:choice
   #:acond
   #:flatten-n
   #:reduce-binary-index
   #:min-index
   #:max-index
   #:split
   #:ith
   #:sigmoid
   #:all-eq-lengths
   #:read-lines
   #:extend-decision-tree
   ;; from rational-sin-cos.lisp
   #:sin-rational
   #:cos-rational
   ))
