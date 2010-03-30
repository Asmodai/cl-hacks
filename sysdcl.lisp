;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: USER; Base: 10; Lowercase: Yes; -*-
;;;
;;; sysdcl.lisp --- DEFSYSTEM for Lisp Machines
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:20:04 asmodai>
;;; Revision:   12
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Tue Sep  1 19:00:00 2009
;;; Keywords:   Common Lisp Hacks
;;; URL:        http://unixware.kicks-ass.org/
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

#+genera
(progn
  (defsystem CL-HACKS
      (:default-pathname "SYS:CL-HACKS;"
       :journal-directory "SYS:CL-HACKS;PATCH;"
       :pretty-name "Common Lisp Hacks"
       :advertised-in (:herald)
       :patchable t
       :maintain-journals t
       :distribute-binaries t
       :distribute-sources t
       :source-category :basic
       :default-module-type :system)
    (:parallel
      (:serial
	"cl-hacks-internals")))
  
  (defsubsystem cl-hacks-internals
      (:default-pathname "SYS:CL-HACKS;SRC;"
       :source-category :basic)
    (:module pkgdcl ("package"))
    (:module genera ("genera")
	     (:uses-definitions-from pkgdcl))
    (:module ifstar ("ifstar")
	     (:uses-definitions-from pkgdcl))
    (:module macros ("macros")
	     (:uses-definitions-from pkgdcl))
    (:module functions ("functions")
	     (:uses-definitions-from pkgdcl))
    (:module equality ("equal")
	     (:uses-definitions-from macros))
    (:module lists ("lists")
	     (:uses-definitions-from macros))
    (:module seqs ("seqs")
	     (:uses-definitions-from macros))
    (:module symbols ("symbols")
	     (:uses-definitions-from macros))
    (:module strings ("strings")
	     (:uses-definitions-from macros))
    (:module math ("math")
	     (:uses-definitions-from macros))
    (:module datetime ("datetime")
	     (:uses-definitions-from macros))
    (:module strmatch ("strmatch")
	     (:uses-definitions-from macros))
    (:module random ("random")
	     (:uses-definitions-from pkgdcl))
    (:module iterate ("iterate")
	     (:uses-definitions-from macros))
    (:module impl ("impl")
	     (:uses-definitions-from macros))
    (:module color ("color")
	     (:uses-definitions-from macros))
    (:module mop ("mop")
	     (:uses-definitions-from macros))
    (:module clos ("clos")
	     (:uses-definitions-from mop))
    (:module console ("console")
	     (:uses-definitions-from macros))
    (:module buffinput ("buff-input")
	     (:uses-definitions-from macros))
    (:module bytestream ("byte-stream")
	     (:uses-definitions-from macros))
    (:module collecting ("collecting")
	     (:uses-definitions-from macros))
    (:module dynstate ("dynamic-state")
	     (:uses-definitions-from macros))
    (:module memoize ("memoize")
	     (:uses-definitions-from macros))
    (:module opsys ("os")
	     (:uses-definitions-from macros))
    (:module io ("io")
	     (:uses-definitions-from macros))
    (:module wrapping ("wrapping-standard")
	     (:uses-definitions-from macros))
    (:module code ("version")
	     (:uses-definitions-from pkgdcl))))

;;; sysdcl.lisp ends here
