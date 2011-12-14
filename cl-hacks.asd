;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;;;
;;; cl-hacks.asd --- CL-Hacks ASDF package definition
;;;
;;; Time-stamp: <Wednesday Dec 14, 2011 23:25:32 asmodai>
;;; Revision:   14
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    05 Dec 2011 05:54:30
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
;;;
;;; {{{ Commentary:
;;;
;;; }}}

#+genera
(error "Please do not load this file into a Symbolics system.
This is only for Common Lisp systems that support ASDF.")

(in-package #:common-lisp-user)

(defpackage cl-hacks-system
  (:use #:asdf
        #:common-lisp))

(in-package #:cl-hacks-system)

(defsystem cl-hacks
    :name "Common Lisp Hacks"
    :author "Paul Ward <asmodai@gmail.com>"
    :version "3.0"
    :maintainer "Paul Ward <asmodai@gmail.com>"
    :license "Lisp Lesser General Public License (LLGPL)"
    :description "Various Common Lisp hacks"
    :long-description "CL-Hacks provides some common hacks that I use
    in various Common Lisp code on many implementations, including
    Symbolics Genera."

    ;; On Genera we use our own internal version of FAD... no so with
    ;;; anything else.
    :depends-on (:cl-fad)

    :components
    ((:module :sys
              :components
              ((:file "package")
               (:file "definitions" :depends-on ("package"))
               (:file "binding" :depends-on ("package"
                                             "definitions"))
               (:file "symbolics" :depends-on ("package"
                                               "binding"))
               (:file "symbols" :depends-on ("package"
                                             "symbolics"))
               (:file "functions" :depends-on ("package"
                                               "symbols"))
               (:file "ifstar" :depends-on ("package"
                                            "functions"))
               (:file "anaphoric" :depends-on ("package"))
               (:file "macros" :depends-on ("package"
                                            "functions"))
               (:file "lists" :depends-on ("package"
                                           "symbols"
                                           "functions"))
               (:file "control-flow" :depends-on ("package"
                                                  "macros"
                                                  "functions"))
               (:file "looping" :depends-on ("package"
                                             "macros"
                                             "functions"))
               (:file "types" :depends-on ("package"
                                           "macros"
                                           "lists"))
               (:file "arrays" :depends-on ("package"
                                            "macros"
                                            "lists"))
               (:file "sequences" :depends-on ("package"
                                               "macros"
                                               "functions"))
               (:file "hash-tables" :depends-on ("package"
                                                 "macros"
                                                 "functions"))
               (:file "documentation" :depends-on ("package"
                                                   "definitions"
                                                   "binding"
                                                   "symbolics"
                                                   "symbols"
                                                   "functions"
                                                   "ifstar"
                                                   "anaphoric"
                                                   "macros"
                                                   "lists"
                                                   "control-flow"
                                                   "looping"
                                                   "types"
                                                   "arrays"
                                                   "sequences"
                                                   "hash-tables"))))
     (:module :mop
              :depends-on (:sys)
              :components
              ((:file "package")
               (:file "mop-extensions" :depends-on ("package"))))
     (:module :clos
              :depends-on (:sys :mop)
              :components
              ((:file "package")
               (:file "classes" :depends-on ("package"))
               (:file "wrapping-standard" :depends-on ("package"))))
     (:module :src
              :depends-on (:sys :mop :clos)
              :components
              ((:file "package")
               (:file "symbols" :depends-on ("package"))
               (:file "strings" :depends-on ("package"))
               (:file "buff-input" :depends-on ("package"))
               (:file "byte-stream" :depends-on ("package"))
               (:file "collecting" :depends-on ("package"))
               (:file "console" :depends-on ("package"))
               (:file "datetime" :depends-on ("package"))
               (:file "dynamic-state" :depends-on ("package"))
               (:file "equal" :depends-on ("package"))
               (:file "for" :depends-on ("package"))
               (:file "glex" :depends-on ("package"))
               (:file "implementations" :depends-on ("package"))
               (:file "io" :depends-on ("package"))
               (:file "iterate" :depends-on ("package"))
               (:file "math" :depends-on ("package"))
               (:file "lists" :depends-on ("package"))
               (:file "macros" :depends-on ("package"))
               (:file "matrix" :depends-on ("package"))
               (:file "memoize" :depends-on ("package"))
               (:file "mp" :depends-on ("package"))
               (:file "os" :depends-on ("package"))
               (:file "random" :depends-on ("package"))
               (:file "version" :depends-on ("package"))
               (:file "exports" :depends-on ("package"))))))

;;; cl-hacks.asd ends here

