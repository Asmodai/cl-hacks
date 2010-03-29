;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: USER; Base: 10; Lowercase: Yes; -*-
;;;
;;; package.lisp --- Package definition
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:31:56 asmodai>
;;; Revision:   44
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Tue Mar  7 17:44:34 2006
;;; Keywords:   Common Lisp Hacks
;;; URL:        http://unixware.kicks-ass.org/
;;;
;;; {{{ License:
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by
;;; the Free Software Foundation, as clarified by the Franz
;;; preamble to the LGPL found in
;;; http://opensource.franz.com/preamble.html.
;;;
;;; This code is distributed in the hope that it will be useful,
;;; but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License can be
;;; found at http://opensource.franz.com/license.html.
;;; If it is not present, you can access it from
;;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;;; version) or write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA  02111-1307  USA
;;;
;;; }}}
;;;
;;; {{{ Commentary:
;;;
;;; It appears that there is no real common "standard" for what
;;; the SOFTWARE-* and MACHINE-* functions return, so... let's define
;;; some functions that return common information across all Lisp
;;; platforms.
;;;
;;; This is probably the only module that resulted in me spending
;;; too much time trying out as many Lisp flavours as I could get my
;;; grubby hands on, now that's something to treasure for the rest
;;; of my life.  Sussman has his suit, I have cl-hacks.
;;;
;;; }}}

(defpackage #:cl-hacks
  (:use #:cl
   #+genera #:clos
   )
  #+genera
  (:import-from :clos-internals      #:validate-superclass
		:clos                #:defclass
		:clos                #:defmethod
		:clos                #:slot-value
		:future-common-lisp  #:nth-value 
		:future-common-lisp  #:declaim
		:future-common-lisp  #:get-setf-expansion
		:future-common-lisp  #:pathname
		:future-common-lisp  #:pathname-host
		:future-common-lisp  #:pathname-directory
		:future-common-lisp  #:pathname-device
		:future-common-lisp  #:open
		:future-common-lisp  #:make-pathname
		:future-common-lisp  #:logical-pathname
		:future-common-lisp  #:translate-logical-pathname
		:future-common-lisp  #:function-lambda-expression
		:future-common-lisp  #:lambda
		:scl                 #:location-boundp
		:scl                 #:fdefinition
		:scl                 #:destructuring-bind
		:condition           #:define-condition
		:cl                  #:load-time-value)
  (:export
   ;;
   ;; console.lisp
   #:*console-messages*
   #:cmsg
   #:cmsg-c
   #:cmsg-add
   #:cmsg-remove
   #:fixme

   ;;
   ;; functions.lisp
   #:_f
   #:compose

   ;;
   ;; memoize.lisp
   #:*memoized-functions*
   #:make-memo
   #:memoize-function
   #:unmemoize-function
   #:unmemoize-functions
   #:clear-memoized-function
   #:clear-memoized-functions
   #:function-memoized-p
   #:def-memoized-function
   #:memoized-labels

   ;;
   ;; macros.lisp
   #:time-iterations
   #:time-seconds
   #:in
   #:mean
   #:with-gensyms
   #:let-if
   #:let-when
   #:aif
   #:awhen
   #:awhile
   #:aand
   #:acond
   #:alambda
   #:it
   #:def-cached-vector
   #:def-cached-instance
   #:with-ignore-errors
   #:ppmx
   #:defconstant*
   #:defvar-unbound
   #:until
   #:while
   #:loop-for
   #:glex-value
   #:defglex
   #:defglpar
   #:pprint-macro
   #:pprint-macro-1

   ;;
   ;; lists.lisp
   #:last-but-one
   #:element-at
   #:flatten-list
   #:list-palindromic-p
   #:compress-list
   #:pack-list
   #:run-length-encode
   #:run-length-decode
   #:duplicate-list
   #:replicate-elem
   #:drop-every-nth
   #:split-list-before
   #:split-list-after
   #:split-list
   #:slice-list
   #:rotate-list
   #:remove-at
   #:insert-at
   #:select-random
   #:random-permutation
   #:combination
   #:remove-list
   #:group-elements
   #:lfsort
   #:mklist
   #:map-and-remove-nils
   #:filter-list
   #:append-list
   #:remove-from-tree-if
   #:find-in-tree
   #:remove-keyword
   #:remove-keywords
   #:mapappend
   #:mapcar-append-string-nontailrec
   #:mapcar-append-string
   #:mapcar2-append-string-nontailrec
   #:mapcar2-append-string
   #:append-sublists
   #:alist-elem-p
   #:alistp
   #:get-alist
   #:udate-alist
   #:alist-plist
   #:plist-alist
   #:update-plist
   #:unique-slot-values

   ;;
   ;; symbols.lisp
   #:ensure-keyword
   #:ensure-keyword-upcase
   #:ensure-keyword-default-case
   #:concat-symbol
   #:concat-symbol-pkg
   #:show
   #:show-variables
   #:show-functions

   ;;
   ;; strings.lisp
   #-genera #:string-append
   #:list-to-string
   #:count-string-words
   #:position-char
   #:position-not-char
   #:delimited-string-to-list
   #:list-to-delimited-string
   #:string-invert
   #:add-sql-quotes
   #:escape-backslashes
   #:substitute-string-for-char
   #:string-substitute
   #:string-trim-last-character
   #:nstring-trim-last-character
   #:string-hash
   #:is-string-empty
   #:*whitespace-chars*
   #:is-char-whitespace
   #:is-string-whitespace
   #:string-right-trim-whitespace
   #:string-left-trim-whitespace
   #:string-trim-whitespace
   #:replaced-string-length
   #:substitute-chars-strings
   #:escape-xml-string
   #:make-usb8-array
   #:usb8-array-to-string
   #:string-to-usb8-array
   #:concat-separated-strings
   #:only-null-list-elements-p
   #:print-separated-strings
   #:integer-string
   #:fast-string-search
   #:string-delimited-string-to-list
   #:string-to-list-skip-delimiter
   #:string-starts-with
   #:count-string-char
   #:count-string-char-if
   #:non-alphanumericp
   #:hexchar
   #:charhex
   #:binary-sequence-to-hex-string
   #:encode-uri-string
   #:decode-uri-string
   #:uri-query-to-alist
   #:random-char
   #:random-string
   #:first-char
   #:last-char
   #:ensure-string
   #:string-right-trim-one-char
   #:remove-char-string
   #:strip-string-ending
   #:string-maybe-shorten
   #:string-elide
   #-genera #:shrink-vector
   #:lex-string
   #:split-alphanumeric-string
   #:trim-non-alphanumeric
   #:collapse-whitespace
   #:string->list
   #:reverse-string
   #:strip-non-alphanumeric
   #:string-palindromic-p
   
   ;;
   ;; strmatch.lisp
   #:score-multiword-search
   #:multiword-match

   ;;
   ;; seqs.lisp
   #:nsubseq

   ;;
   ;; math.lisp
   #:deriv
   #:sin^
   #:ensure-integer
   #:histogram
   #:fixnum-width
   #:scaled-epsilon
   #:sinc
   #:numbers-within-percentage

   ;;
   ;; equal.lisp
   #:generalized-equal
   #:generalized-equal-function
   #:generalized-equal-hash-table
   #:generalized-equal-fielded-object
   #:class-slot-names
   #:structure-slot-names
   #:function-to-string
   
   ;;
   ;; collecting.lisp
   #:collecting
   #:with-collectors

   ;;
   ;; dynamic-state.lisp
   #:define-dynamic-state

   ;;
   ;; iterate.lisp
   #:+tr-implementation-p+
   #:iterate
   #:iterate/labels
   #:iterate/tag

   ;;
   ;; wrapping-standard.lisp
   #:wrapping-standard

   ;;
   ;; color.lisp
   #:hsv->rgb
   #:hsv255->rgb255
   #:rgb->hsv
   #:rgb255->hsv255
   #:hsv-equal
   #:hsv255-equal
   #:hsv-similar
   #:hsv255-similar
   #:hue-difference
   #:hue-difference-fixnum
   
   ;;
   ;; random.lisp
   #:seed-random-generator
   #:random-choice

   ;;
   ;; datetime.lisp
   #:pretty-date
   #:pretty-date-ut
   #:date-string
   #:print-seconds
   #:*posix-epoch*
   #:posix-time-to-utime
   #:utime-to-posix-time
   #:*monthnames*
   #:monthname
   #:*daynames*
   #:dayname
   #:day-of-week

   ;;
   ;; io.lisp
   #:print-file-contents
   #:read-stream-to-string
   #:read-file-to-string
   #:read-file-to-usb8-array
   #:read-stream-to-strings
   #:file-subst
   #:print-n-chars
   #:print-n-strings
   #:indent-spaces
   #:intent-html-spaces
   #:print-list
   #:print-rows
   #:bref
   #:new-buf
   #:buf-insert
   #:buf-pop
   #:buf-next
   #:buf-reset
   #:buf-clear
   #:buf-flush
   #:stream-subst
   #:write-fixnum
   #:null-output-stream
   #:directory-tree
   #:with-utime-decoding
   #:is-dst
   #:with-utime-decoding-utc-offset
   #:write-utime-hms
   #:write-utime-hms-stream
   #:write-utime-hm
   #:write-utime-hm-stream
   #:write-utime-ymdhms
   #:write-utime-ymdhms-stream
   #:write-utime-ymdhm
   #:write-utime-ymdhm-stream
   #:copy-binary-stream

   ;;
   ;; buff-input.lisp
   #:+max-field+
   #:+max-fields-per-line+
   #:+field-delim+
   #:+eof-char+
   #:+newline+
   #:make-fields-buffer
   #:read-buffered-fields
   #:make-field-buffer2
   #:read-buffered-fields2
   #:bfield
   #:+max-line+

   ;;
   ;; impl.lisp
   #:canonicalize-directory-name
   #:probe-directory
   #:cwd
   #:quit
   #:command-line-arguments
   #-genera #:copy-file

   ;;
   ;; version.lisp
   #:common-lisp-implementation-type
   #:common-software-type
   #:common-sofware-version
   #:common-machine-type
   #:common-machine-version
   
   ;;
   ;; byte-stream.lisp

   ;;
   ;; signals.lisp

   ;;
   ;; clos.lisp
   #:singleton-class
   #:reset-singleton-classes
   #:define-abstract-class
   #:define-final-class     
))

;;; This is so we can tell CL-HACKS has been loaded
(eval-when (:load-toplevel :compile-toplevel :execute)
  (if (not (member :cl-hacks cl:*features*))
      (pushnew :cl-hacks cl:*features*)))

;;; package.lisp ends here
