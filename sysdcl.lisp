;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-
;;;
;;; sysdcl.lisp --- DEFSYSTEM for Symbolics gear
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 06:30:20 asmodai>
;;; Revision:   6
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    25 Nov 2011 19:18:50
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
(error "This requires Symbolics Genera.")

(defsubsystem cl-hacks-internals
    (:pretty-name "CL Hacks internals"
     :default-pathname "cl-hacks:sys;")
  (:module clhi-package ("package"))
  (:module clhi-genera ("genera"))
  (:module clhi-stage1 ("definitions"
                        "binding"
                        "symbols"
                        "functions"
                        "ifstar"
                        "anaphoric"
                        "conditions"
                        "macros"
                        "lists")
           (:uses-definitions-from clhi-package))
  (:module clhi-stage2 ("control-flow"
                        "looping"
                        "types"
                        "arrays"
                        "sequences"
                        "hash-tables")
           (:uses-definitions-from clhi-stage1))
  (:module clhi-stage3 ("features")
           (:uses-definitions-from clhi-package clhi-stage1
                                   clhi-stage2))
  (:module clhi-zwei ("emacs")
           (:uses-definitions-from clhi-stage1 clhi-stage2
                                   clhi-stage3))
  (:serial clhi-package clhi-genera clhi-stage1 clhi-stage2 
           clhi-stage3 clhi-zwei))

(defsubsystem cl-hacks-fad
    (:pretty-name "CL Hacks Files and Directories"
     :default-pathname "cl-hacks:fad;")
  (:module fad-package ("package"))
  (:module fad-sources ("fad")
           (:uses-definitions-from fad-package))
  (:serial fad-package fad-sources))

(defsubsystem cl-hacks-uuid
    (:pretty-name "CL Hacks UUID"
     :default-pathname "cl-hacks:uuid;")
  (:module uuid-package ("package"))
  (:module uuid-source ("uuid")
           (:uses-definitions-from uuid-package))
  (:serial uuid-package uuid-source))

(defsubsystem cl-hacks-mop
    (:pretty-name "CL Hacks MOP"
     :default-pathname "cl-hacks:mop;")
  (:module mop-package ("package"))
  (:module mop-sources ("mop-extensions")
           (:uses-definitions-from mop-package))
  (:serial mop-package mop-sources))

(defsubsystem cl-hacks-clos
    (:pretty-name "CL Hacks CLOS"
     :default-pathname "cl-hacks:clos;")
  (:module clos-package ("package"))
  (:module clos-sources ("classes"
                         "wrapping-standard")
           (:uses-definitions-from clos-package))
  (:serial clos-package clos-sources))

(defsubsystem cl-hacks-sys
    (:pretty-name "CL Hacks source"
     :default-pathname "cl-hacks:src;")
  (:module sys-package ("package"))
  (:module sys-sources ("symbols"
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
                        ;; mp
                        "os"
                        "random"
                        "serialize"
                        "simple-actor"
                        "version"
                        "exports")
           (:uses-definitions-from sys-package))
  (:serial sys-package sys-sources))
    
(defsystem cl-hacks
    (:pretty-name "Common Lisp Hacks"
     :short-name "CL-Hacks"
     ;;
     ;; Defaults
     :default-pathname "cl-hacks:site;"
     ;;
     ;; Journaling
     :maintain-journals t
     :journal-directory "cl-hacks:patch;"
     :patchable t
     ;;
     ;; Misc
     :advertised-in :herald
     :initial-status :released
     :source-category :basic
     :distribute-sources t
     :distribute-binaries t)
  ;;
  ;; Module definitions
  (:module internals (cl-hacks-internals) (:type :system))
  (:module fad (cl-hacks-fad) (:type :system))
  (:module uuid (cl-hacks-uuid) (:type :system))
  (:module mop (cl-hacks-mop) (:type :system))
  (:module clos (cl-hacks-clos) (:type :system))
  (:module hacks (cl-hacks-sys) (:type :system))
  ;;
  ;; What must be done to load the modules
  (:serial internals fad mop clos hacks))

;;; sysdcl.lisp ends here
