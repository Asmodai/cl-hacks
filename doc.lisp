;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;;;
;;; doc.lisp --- DEFSYSTEM for documentation
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:59:14 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    25 Nov 2011 22:02:10
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

(defsystem cl-hacks-doc
    (:pretty-name "Common Lisp Hacks Documentation"
     ;;
     ;; Defaults
     :default-pathname "cl-hacks:doc;"
     ;;
     ;; Journaling
     :patchable t
     ;;
     ;; Misc
     :advertised-in nil
     :default-module-type :sage
     :distribute-sources t
     :distribute-binaries t
     :source-category :basic)
  ;;
  (:module cl-hacks ("cl-hacks" "anaphoric")))

(sage::register-book "Common Lisp Hacks"
                     :document-type 'sage::3symanual
                     :highest-structure-level 'sage::chapter
                     :doc# ""
                     :mnemonic ""
                     :releaseversion "Genera 8.0"
                     :authorgroup "Paul Ward"
                     :symcopy nil
                     :mitcopy nil)

;;; doc.lisp ends here
