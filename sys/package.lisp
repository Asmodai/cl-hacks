;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;;;
;;; package.lisp --- Package definition
;;;
;;; Time-stamp: <Monday Dec  5, 2011 05:50:27 asmodai>
;;; Revision:   11
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    30 Nov 2011 23:44:53
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;; {{{ License:
;;;
;;; This code is free software; you can redistribute it and/or modify
;;; it under the terms of the version 2.1 of the GNU Lesser General
;;; Public License as published by the Free Software Foundation, as
;;; clarified by the Franz preamble to the LGPL found in
;;; http://opensource.franz.com/preamble.html.
;;;
;;; This code is distributed in the hope that it will be useful, but
;;; without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License can be found
;;; at http://opensource.franz.com/license.html. If it is not present,
;;; you can access it from http://www.gnu.org/copyleft/lesser.txt
;;; (until superseded by a newer  version) or write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite  330, Boston, MA
;;; 02111-1307  USA
;;;
;;; }}}
;;; {{{ Commentary:
;;;
;;; }}}

#-genera
(in-package #:cl-user)

(defpackage #:cl-hacks-internals
  (:nicknames #:clhi)
  (:use #-genera        #:common-lisp
        #+genera        #:future-common-lisp)
;;;
;;; Optional imports
;;; {{{ LispWorks imports:

  #+lispworks
  (:import-from :common-lisp-user #:with-unique-names)

;;; }}}
;;; {{{ Symbolics Genera imports:

  #+genera
  (:shadowing-import-from #:scl #:lambda)

  #+genera
  (:shadowing-import-from :zl  #:named-lambda
                               #:rest1
                               #:mem
                               #:selectq)

  #+genera
  (:shadowing-import-from :si  #:array-mem
                               #:*whitespace*)

  #+genera
  (:shadowing-import-from :lt  #:named-constant-p
                               #:evaluate-constant)

  #+genera
  (:shadowing-import-from :cli #:coerce-string-arg
                               #:string-pluralize-to-stream
                               #:*compound-separators*
                               #:*pronounce-aitch*)

  #+genera
  (:shadowing-import-from :scl #:defselect
                               #:defsubst
                               #:once-only
                               #:string-append
                               #:string-length
                               #:substring
                               #:nsubstring
                               #:char-flipcase
                               #:string-flipcase
                               #:nstring-flipcase
                               #:string-capitalize-word
                               #:nstring-capitalize-word
                               #:string-compare
                               #:string-exact-compare
                               #:string-reverse
                               #:string-nreverse
                               #:string-search-char
                               #:string-search-exact-char
                               #:string-search-not-char
                               #:string-search-not-exact-char
                               #:string-search-set
                               #:string-search-not-set
                               #:string-search
                               #:string-search-exact
                               #:string-pluralize
                               #:*string-a-or-an-exceptions-list*
                               #:string-a-or-an)

  #+genera
  (:shadowing-import-from :zwei #:defindentation
                                #:get-indentation)

;;; }}}
;;;
;;; Exports
  (:export

;;; {{{ anaphoric.lisp:
   
   #:aif
   #:awhen
   #:awhile
   #:aand
   #:acond
   #:alambda
   #:ablock
   #:aif2
   #:awhen2
   #:awhile2
   #:acond2

;;; }}}
;;; {{{ arrays.lisp:

   #:copy-array

;;; }}}
;;; {{{ binding.lisp:
   
   #:if-let
   #:when-let
   #:when-let*

;;; }}}
;;; {{{ conditions.lisp:

   #:code
   #:case-error
   #:not-implemented
   #:required-argument-missing
   #:required-argument
   #:simple-style-warning
   #:simple-reader-error
   #:simple-parse-error
   #:simple-program-error
   #:ignore-some-conditions
   #:unwind-protect-case

;;; }}}
;;; {{{ control-flow.lisp:

   #:switch
   #:eswitch
   #:cswitch
   #:whichever
   #:xor
   #:nth-value-or

;;; }}}
;;; {{{ definitions.lisp:

   #:define-constant
   #:defconstant*
   #:defcustom
   #:defconst
   #:make-typed-array
   
;;; }}}
;;; {{{ documentation.lisp:

   #:define-documentation

;;; }}}
;;; {{{ emacs.lisp:

   #:defindentation
   #:get-indentation

;;; }}}
;;; {{{ functions.lisp:

   #:ensure-function
   #:disjoin
   #:conjoin
   #:compose
   #:multiple-value-compose
   #:curry
   #:rcurry

;;; }}}
;;; {{{ features.lisp:

   #:featurep

;;; }}}
;;; {{{ ifstar.lisp:

   #:if*-keyword-list
   #:if*

;;; }}}
;;; {{{ lists.lisp:

   #:alist-plist
   #:plist-alist
   #:malformed-plist
   #:doplist
   #:appendf
   #:nconcf
   #:unionf
   #:nunionf
   #:circular-list
   #:list-to-circular-list
   #:circular-list-p
   #:circular-tree-p
   #:proper-list-p
   #:proper-list
   #:lastcar
   #:make-circular-list
   #:ensure-carp
   #:ensure-cons
   #:ensure-list
   #:remove-from-plist
   #:delete-from-plist
   #:remove-from-plistf
   #:delete-from-plistf
   #:sans
   #:mappend
   #:setp
   #:set-equal
   #:map-product
   #:flatten

;;; }}}
;;; {{{ looping.lisp:

   #:til
   #:while
   #:loop-for

;;; }}}
;;; {{{ macros.lisp:

   #:with-gensyms
   #:with-unique-names
   #:once-only
   #:with-ignore-errors
   #:discard-docstring
   #:parse-body
   #:parse-ordinary-lambda-list
   
;;; }}}
;;; {{{ sequences.lisp:

   #:rotate
   #:shuffle
   #:random-elt
   #:removef
   #:deletef
   #:proper-sequence
   #:emptyp
   #:length=
   #:sequence-of-length-p
   #:copy-sequence
   #:first-elt
   #:last-elt
   #:starts-with-subseq
   #:ends-with-subseq
   #:starts-with
   #:ends-with
   #:map-combinations
   #:map-permutations
   #:map-derangements
   #:nsubseq

;;; }}}
;;; {{{ symbolics.lisp:

   #:named-lambda
   #:rest1
   #:mem
   #:selectq
   #:array-mem
   #:named-constant-p
   #:evaluate-constant
   #:coerce-string-arg
   #:defselect
   #:defsubst
   #:string-append
   #:string-length
   #:substring
   #:nsubstring
   #:char-flipcase
   #:string-flipcase
   #:nstring-flipcase
   #:string-capitalize-words
   #:nstring-capitalize-words
   #:string-compare
   #:string-exact-compare
   #:string-reverse
   #:string-nreverse
   #:string-search-char
   #:string-search-exact-char
   #:string-search-not-char
   #:string-search-not-exact-char
   #:string-search-set
   #:string-search-not-set
   #:string-search
   #:string-search-exact
   #:*compound-separators*
   #:*whitespace*
   #:string-pluralize
   #:string-pluralize-to-stream
   #:*pronounce-aitch*
   #:*string-a-or-an-exceptions-list*
   #:string-a-or-an

;;; }}}
;;; {{{ symbols.lisp:

   #:ensure-symbol
   #:maybe-intern
   #:format-symbol
   #:make-keyword
   #:make-gensym
   #:make-gensym-list
   #:symbolicate

;;; }}}
;;; {{{ types.lisp:

   #:array-index
   #:array-length
   #:string-designator
   #:of-type
   #:type=
   #:coercef

   ;; CDR5 types
   #:negative-double-float
   #:negative-fixnum-p
   #:negative-float
   #:negative-float-p
   #:negative-long-float
   #:negative-long-float-p
   #:negative-rational 
   #:negative-rational-p
   #:negative-real
   #:negative-single-float-p
   #:non-negative-double-float
   #:non-negative-double-float-p
   #:non-negative-fixnum
   #:non-negative-fixnum-p
   #:non-negative-float
   #:non-negative-float-p
   #:non-negative-integer-p
   #:non-negative-long-float
   #:non-negative-rational
   #:non-negative-real-p
   #:non-negative-short-float-p
   #:non-negative-single-float
   #:non-negative-single-float-p
   #:non-positive-double-float
   #:non-positive-double-float-p
   #:non-positive-fixnum
   #:non-positive-fixnum-p
   #:non-positive-float
   #:non-positive-float-p
   #:non-positive-integer
   #:non-positive-rational
   #:non-positive-real
   #:non-positive-real-p
   #:non-positive-short-float
   #:non-positive-short-float-p
   #:non-positive-single-float-p
   #:positive-double-float
   #:positive-double-float-p
   #:positive-fixnum
   #:positive-fixnum-p
   #:positive-float
   #:positive-float-p
   #:positive-integer
   #:positive-rational
   #:positive-real
   #:positive-real-p
   #:positive-short-float
   #:positive-short-float-p
   #:positive-single-float
   #:positive-single-float-p
   #:negative-double-float-p
   #:negative-fixnum
   #:negative-integer
   #:negative-integer-p
   #:negative-real-p
   #:negative-short-float
   #:negative-short-float-p
   #:negative-single-float
   #:non-negative-integer
   #:non-negative-long-float-p
   #:non-negative-rational-p
   #:non-negative-real
   #:non-negative-short-float
   #:non-positive-integer-p
   #:non-positive-long-float
   #:non-positive-long-float-p
   #:non-positive-rational-p
   #:non-positive-single-float
   #:positive-integer-p
   #:positive-long-float
   #:positive-long-float-p
   #:positive-rational-p

;;; }}}

   ))
;;; package.lisp ends here
