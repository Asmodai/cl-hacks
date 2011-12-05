;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-
;;;
;;; This is a Genera package that implements most of the CL UUID package
;;; except the components that require Ironclad.  This means you only
;;; have V1, V2 and V4 UUIDs.

(defpackage #:cl-hacks-uuid
  (:use #-genera #:common-lisp
        #+genera #:future-common-lisp)
  (:nicknames #:uuid)
  (:export
    #:uuid
    #:*ticks-per-count*
    #:make-null-uuid
    #:make-uuid-from-string
    #:make-v1-uuid
    #:make-v3-uuid
    #:make-v4-uuid
    #:make-v5-uuid
    #:+namespace-dns+
    #:+namespace-url+
    #:+namespace-oid+
    #:+namespace-x500+
    #:print-bytes
    #:print-uuid
    #:uuid-to-byte-array))

;;; EOF
