;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;;;
;;; package.lisp --- CL-HACKS package definition
;;;
;;; Time-stamp: <Sunday Feb 19, 2012 07:36:28 asmodai>
;;; Revision:   16
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Dec 2011 04:26:02
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
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
;;;}}}
;;;{{{ Commentary:
;;;
;;;}}}

#-genera
(in-package #:cl-user)

(defpackage #:cl-hacks
  (:use #-genera #:common-lisp
        #+genera #:future-common-lisp
                 #:cl-hacks-internals
                 #:cl-fad
                 #:cl-hacks-mop
                 #:cl-hacks-clos)
  ;;
  ;; Genera hacks
  #+genera
  (:import-from #:cl #:lambda)
  ;;
  ;; Exports
  (:export

;;;{{{ buff-input.lisp:
   
   #:+max-field+
   #:+max-fields-per-line+
   #:+field-delim+
   #:+eof-char+
   #:+newline+

   #:make-fields-buffer
   #:read-buffered-fields
   #:field-buffers
   #:make-fields-buffer2
   #:read-buffered-fields2
   #:bfield
   #:+max-line+
   #:read-buffered-line

;;;}}}
;;;{{{ byte-stream.lisp:

   #+(or cmu sbcl) #:byte-array-output-stream
   #+(or cmu sbcl) #:byte-array-bout
   #+(or cmu sbcl) #:byte-array-out-misc
   #+(or cmu sbcl) #:get-output-stream-data
   #+(or cmu sbcl) #:dump-output-stream-data

   #+(or cmu sbcl) #:byte-array-input-stream
   #+(or cmu sbcl) #:byte-array-inch
   #+(or cmu sbcl) #:byte-array-binch
   #+(or cmu sbcl) #:byte-array-stream-read-n-bytes
   #+(or cmu sbcl) #:byte-array-in-misc
   #+(or cmu sbcl) #:make-byte-array-input-stream

   #+allegro #:extendable-buffer-output-stream
   #+allegro #:make-byte-array-output-stream
   #+allegro #:get-output-stream-data
   #+allegro #:dump-output-stream-data
   #+allegro #:make-byte-array-input-stream

;;;}}}
;;;{{{ collecting.lisp:

   #:collecting
   #:with-collectors

;;;}}}
;;;{{{ console.lisp:

   #:*console-messages*
   #:*console-message-types*
   
   #:cmsg
   #:cmsg-c
   #:cmsg-add
   #:cmsg-remove
   #:cmsg-list
   #:fixme

;;;}}}
;;;{{{ datetime.lisp:

   #:timestamp
   #:subzone
   #:timezone
   #:timezone-offset
   #:time-of-day
   #:date
   #:invalid-timezone-file
   #:invalid-time-specification
   #:invalid-timestring
   #:*default-timezone-repository-path*

   #:+month-names+
   #:+short-month-names+
   #:+day-names+
   #:+day-names-as-keywords+
   #:+short-day-names+
   #:+months-per-year+
   #:+days-per-week+
   #:+hours-per-day+
   #:+minutes-per-day+
   #:+minutes-per-hour+
   #:+seconds-per-day+
   #:+seconds-per-hour+
   #:+seconds-per-minute+
   #:+usecs-per-day+
   #:+iso-8601-format+
   #:+rfc3339-format+
   #:+rfc3339-format/date-only+
   #:+asctime-format+
   #:+rfc-1123-format+
   #:+rotated-month-days-without-leap-day+
   #:+rotated-month-offsets-without-leap-day+
   #:+astronomical-julian-date-offset+
   #:+modified-julian-date-offset+

   #:+utc-zone+
   #:+gmt-zone+
   #:+none-zone+

   #:define-timezone
   #:*default-timezone*
   #:*location-name->timezone*
   #:*abbreviated-subzone-name->timezone-list*
   #:find-timezone-by-location-name
   #:timezone=
   #:reread-timezone-repository

   #:make-timestamp
   #:clone-timestamp
   #:transition-position
   #:timestamp-subtimezone
   #:timestamp-minimize-part
   #:timestamp-maximize-part
   #:with-decoded-timestamp

   #:days-in-month

   #:adjust-timestamp
   #:adjust-timestamp!

   #:timestamp-whole-year-difference
   #:timestamp-difference
   #:timestamp+
   #:timestamp-
   #:timestamp-day-of-week
   #:valid-timestamp-p
   #:encode-timestamp-into-values
   #:encode-timestamp
   #:universal-to-timestamp
   #:timestamp-to-universal
   #:unix-to-timestamp
   #:timestamp-values-to-unix
   #:timestamp-to-unix
   #:now
   #:today
   
   #:timestamp<
   #:timestamp<=
   #:timestamp>
   #:timestamp>=
   #:timestamp=
   #:timestamp/=

   #:contest
   #:timestamp-minimum
   #:timestamp-maximum

   #:years-to-days
   #:days-to-years

   #:decode-timestamp
   #:timestamp-year
   #:timestamp-century
   #:timestamp-millennium
   #:timestamp-decade
   #:timestamp-month
   #:timestamp-day
   #:timestamp-hour
   #:timestamp-minute
   #:timestamp-second
   #:timestamp-microsecond
   #:timestamp-millisecond
   #:split-timestring

   #:parse-rfc3339-timestring
   #:parse-timestring
   #:ordinalize
   #:format-timestring
   #:format-rfc1123-timestring
   #:to-rfc1123-timestring
   #:format-rfc3339-timestring
   #:to-rfc3339-timestring

   #:enable-read-macros

   #:astronomical-julian-date
   #:modified-julian-date
   #:day-of-week
   #:date-string
   #:pretty-date
   #:monthname
   #:dayname
   
   #:*weekday-strings*
   #:*month-strings*
   #:*zone-strings*
   #:*special-strings*
   #:*default-date-time-patterns*
   #:*http-date-time-patterns*

   #:decoded-time
   #:make-default-time
   #:convert-to-unitime
   #:set-current-value
   #:parse-yesterday
   #:parse-today
   #:parse-tomorrow
   #:parse-now
   #:am-pm
   #:noon-midn
   #:weekday
   #:zone
   #:izone
   #:special-string-p
   #:secondp
   #:minute
   #:hour
   #:day
   #:year
   #:time-divider
   #:date-divider
   #:parse-time
   
;;;}}}
;;;{{{ dynamic-state.lisp:

   #:define-dynamic-state

;;;}}}
;;;{{{ equal.lisp:

   #:generalized-equal
   #:generalized-equal-function
   #:generalized-equal-array
   #:generalized-equal-hash-table
   #:generalized-equal-fielded-object
   #:class-slot-names
   #:structure-slot-names
   #:function-to-string

;;;}}}
;;;{{{ for.lisp:

   #:iter
   #:next
   #:for
   #:range
   #:gather

;;;}}}
;;;{{{ glex.lisp:

   #:unbound-global-lexical
   #:glex-value
   #:defglex
   #:defglpar

;;;}}}
;;;{{{ implementations.lisp:

   #:canonicalize-directory-name
   #:probe-directory
   #:cwd
   #:quit
   #:command-line-arguments
   #-genera #:copy-file

;;;}}}
;;;{{{ io.lisp:

   #:print-file-contents
   #:read-stream-to-string
   #:read-file-to-string
   #:read-file-to-usb8-array
   #:read-stream-to-strings
   #:read-file-to-strings
   #:file-subst
   #:print-n-chars
   #:print-n-strings
   #:indent-spaces
   #:indent-html-spaces
   #:print-list
   #:print-rows
   #:buf
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
   #:with-input-from-file
   #:with-output-to-file
   #:read-file-into-string
   #:write-file-into-string
   #:copy-buffer-stream

;;;}}}
;;;{{{ iterate.lisp:

   #:iterate
   #:iterate/labels
   #:iterate/tag

;;;}}}
;;;{{{ lists.lisp:

   #:last-but-one
   #:element-at
   #:list-palindromic-p
   #:compress-list
   #:pack-list
   #:run-length-encode
   #:run-length-encode/simplified
   #:run-length-decode
   #:replicate-list
   #:duplicate-list
   #:drop-every-nth
   #:split-list
   #:slice-list
   #:rotate-list
   #:remove-at
   #:insert-at
   #:numeric-range
   #:random-select
   #:lottery-select
   #:random-permutation
   #:list-combinations
   #:list-group
   #:list-group3
   #:lsort
   #:lfsort
   #:mklist
   #:map-and-remove-nils
   #:filter-list
   #:append-list
   #:remove-from-tree-if
   #:find-in-tree
   #:remove-keyword
   #:remove-keywords
   #:mapcar-append-string-nontailrec
   #:mapcar-append-string
   #:mapcar/2-append-string-nontailrec
   #:mapcar/2-append-string
   #:append-sublists
   #:alist-elem-p
   #:alistp
   #:get-alist
   #:unique-slot-values

;;;}}}
;;;{{{ macros.lisp:

   #:with-each-stream-line
   #:with-each-file-line
   #:in
   #:mean
   #:time-seconds
   #:time-iterations
   #:def-cached-vector
   #:def-cached-instance
   #:pprint-macro
   #:pprint-macro-1
   #:ppmx

;;;}}}
;;;{{{ math.lisp:

   #:primep
   #:coprime
   #:totient-phi
   #:compute-primes-to
   #:factorize
   #:prime-factors
   #:prime-factors*
   #:goldbach
   #:deriv
   #:sin^
   #:ensure-integer
   #:make-histogram
   #:histogram
   #:fixnum-width
   #:scaled-epsilon
   #:sinc
   #:numbers-within-percentage
   #:print-float-units
   #:simplify
   #:compare-terms
   #:order
   #:not-sortablep
   #:update-terms
   #:add
   #:multiply
   #:expoentiate
   #:logarithm
   #:negate
   #:reciprocal

;;;}}}
;;;{{{ matrix.lisp:

   #:matrix-minor
   #:matrix-rows
   #:matrix-columns
   #:matrix-row
   #:matrix-column
   #:matrix-element
   #:matrix-rank
   #:matrix-augment
   #:matrix-split
   #:matrix-apply
   #:matrixp
   #:matrix-squarep
   #:matrix-upper-triangularp
   #:matrix-symmetricp
   #:matrix-orthogonalp
   #:matrix-invertiblep
   #:matrix-zero
   #:matrix-ones
   #:matrix-identity
   #:matrix-random
   #:matrix-multiply
   #:matrix-add
   #:matrix-normalize
   #:matrix-qr
   #:matrix-lu
   #:matrix-transpose
   #:matrix-count-zeros
   #:matrix-determinant
   #:matrix-row-swap
   #:print-matrix
   #:calculate-column-width
   #:correct-near-zero
   #:matrix-eigenvalues
   #:matrix-ref
   #:matrix-rref
   #:matrix-gauss-jordan
   #:matrix-gaussian
   #:matrix-gaussian2
   #:matrix-inverse
   #:standard-basis-row
   #:matrix-nullspace
   #:matrix-columnspace
   #:print-matrix-four-subspaces

;;;}}}
;;;{{{ memoize.lisp:

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

;;;}}}
;;;{{{ mp.lisp:

   ;;; TODO: This requires hackery for Genera
   #-genera #:mp-make-lock
   #-genera #:mp-with-lock-held
   #-genera #:mp-with-recursive-lock-held
   #-genera #:make-process
   #-genera #:destroy-process
   #-genera #:process-active-p

;;;}}}
;;;{{{ os.lisp:

   #:command-output
   #:run-system-command
   #:delete-directory-and-files
   #:file-size
   #:getpid

;;;}}}
;;;{{{ random.lisp:

   #:seed-random-generator
   #:random-choice

;;;}}}
;;;{{{ serialize.lisp:

   #:serializer
   #:save-objects
   #:load-objects
   #:open-serializer
   #:close-serializer
   #:force-serializer-output
   
;;;}}}
;;;{{{ simple-actor.lisp:

   #:simple-actor
   #:run-actor-function
   #:stop-actor
   #:start-actor
   #:actor-running-p
   
;;;}}}
;;;{{{ strings.lisp:

   #:list-to-string
   #:count-string-words
   #:position-char
   #:position-not-char
   #:delimited-string-to-list
   #:list-to-delimited-string
   #:add-sql-quotes
   #:escape-backslashes
   #:substitute-string-for-char
   #:string-substitute
   #:string-trim-last-character
   #:nstring-trim-last-character
   #:string-hash
   #:is-string-empty
   #:is-char-whitespace
   #:is-string-whitespace
   #:whitespace-p
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
   #:prefixed-fixnum-string
   #:prefixed-integer-string
   #:integer-string
   #:fast-string-search
   #:string-delimited-string-to-list
   #:string-to-list-skip-delimiter
   #:string-starts-with
   #:count-string-char
   #:count-string-char-if
   #:non-alphanumericp
   #:+hex-chars+
   #:hexchar
   #:charhex
   #:binary-sequence-to-hex-string
   #:encode-uri-string
   #:decode-uri-string
   #:uri-query-to-alist
   #:+unambiguous-charset+
   #:random-char
   #:random-string
   #:first-char
   #:last-char
   #:ensure-string
   #:string-right-trim-one-char
   #:remove-char-string
   #:string-strip-ending
   #:string-maybe-shorten
   #:string-elide
   #:shrink-vector
   #:lex-string
   #:split-alphanumeric-string
   #:trim-non-alphanumeric
   #:collapse-whitespace
   #:string->list
   #:reverse-string
   #:strip-non-alphanumeric
   #:string-palindromic-p
   #:strip-newlines
   #:string-center
   #:read-string-until
   #:read-string-until-quote
   #:read-string-until-comment
   #:read-string-until-whitespace
   #:read-something
   #:read-string-delimited-list
   #:read-string
   #:collect-whitespace
   #:string-beginning-with-p 
   #:string-delimited-by-p

;;;}}}
;;;{{{ symbols.lisp:
   
   #:cl-symbols
   #:cl-variables
   #:cl-functions
   #:string-default-case
   #:concat-symbol-pkg
   #:concat-symbol
   #:ensure-keyword
   #:ensure-keyword-upcase
   #:ensure-keyword-default-case
   #:show
   #:show-variables
   #:show-functions

;;;}}}
;;;{{{ version.lisp:

   #:common-lisp-implementation-type
   #:common-lisp-implementation-version
   #:common-software-type
   #:common-software-version
   #:common-machine-version
   #:common-machine-type
   #:common-machine-instance

;;;}}}

   ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cl-hacks cl:*features*))

;;; package.lisp ends here

