;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;;;
;;; lw-defsys.lisp --- LispWorks system definition
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 06:30:09 asmodai>
;;; Revision:   13
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    30 Nov 2011 23:21:50
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

#-lispworks
(error "This is meant for LispWorks!")

(in-package #:cl-user)

(defsystem cl-hacks-internals
  (:default-pathname "sys/"
   :default-type :lisp-file
   :optimize ((speed 3)
              (space 3)
              (safety 0)))
  :members ("package"
            "definitions"
            "binding"
            "symbolics"
            "symbols"
            "functions"
            "ifstar"
            "anaphoric"
            "conditions"
            "macros"
            "control-flow"
            "looping"
            "lists"
            "types"
            "arrays"
            "sequences"
            "hash-tables"
            "features"
            "emacs"
            "documentation")
  :rules ((:in-order-to :compile :all
           (:requires (:load :serial)))))

(defsystem cl-hacks-fad
  (:default-pathname "fad/"
   :default-type :lisp-file
   :optimize ((speed 3)
              (space 3)
              (safety 0)))
  :members ("package"
            "fad")
  :rules ((:in-order-to :compile :all
           (:requires (:load :serial)))))

(defsystem cl-hacks-mop
  (:default-pathname "mop/"
   :default-type :lisp-file
   :optimize ((speed 3)
              (space 3)
              (safety 0)))
  :members ("package"
            "mop-extensions")
  :rules ((:in-order-to :compile :all
           (:requires (:load :serial)))))

(defsystem cl-hacks-clos
  (:default-pathname "clos/"
   :default-type :lisp-file
   :optimize ((speed 3)
              (space 3)
              (safety 0)))
  :members ("package"
            "classes"
            "wrapping-standard")
  :rules ((:in-order-to :compile :all
           (:requires (:load :serial)))))

(defsystem cl-hacks-sys
  (:default-pathname "src/"
   :default-type :lisp-file
   :optimize ((speed 3)
              (space 3)
              (safety 0)))
  :members ("package"
            "symbols"
            "strings"
            "buff-input"
            "byte-stream"
            "collecting"
            "console"
            "datetime"
            "dynamic-state"
            "equal"
            "for"
            "glex"
            "implementations"
            "io"
            "iterate"
            "math"
            "lists"
            "macros"
            "matrix"
            "memoize"
            "mp"
            "os"
            "random"
            "serialize"
            "simple-actor"
            "version"
            "exports")
  :rules ((:in-order-to :compile :all
           (:requires (:load :serial)))))

(defsystem cl-hacks
  ()
  :members (("cl-hacks-internals" :type :system)
            ("cl-hacks-fad" :type :system)
            ("cl-hacks-mop" :type :system)
            ("cl-hacks-clos" :type :system)
            ("cl-hacks-sys" :type :system))
  :rules ((:in-order-to :compile :all
           (:requires (:load :serial)))))

;;; lw-defsys.lisp ends here
