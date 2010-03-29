;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: COMMON-LISP-USER; Base: 10 -*-
;;;
;;; cl-hacks.asd --- ASDF package definition.
;;;
;;; Time-stamp: <Tuesday Sep  1, 2009 22:40:37 asmodai>
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2002 Keven M. Rosenberg
;;; Copyright (c) 2002 Tim Bradshaw
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Tue Sep  1 19:00:00 2009
;;; Keywords:   Common Lisp CLOS Hacks
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

(in-package #:common-lisp-user)

(defpackage #:cl-hacks-system
  (:use #:asdf #:common-lisp))

(pushnew :cl-hacks *features*)

;; Check for MOP hacks
#+(or allegro cmu clisp lispworks sbcl scl openmcl)
(pushnew :cl-hacks-mop *features*)

(in-package #:cl-hacks-system)

(defsystem cl-hacks
    :name "cl-hacks"
    :author "Paul Ward <asmodai@gmail.com>"
    :version "2.0"
    :maintainer "Paul Ward <asmodai@gmail.com>"
    :licence "GNU Lesser General Public License"
    :description "Lisp utility library for Common Lisp"
    :long-description "cl-hacks provides some common hacks that I use in my
programs :)"
    
    :components
    ((:module :src
              :components
	      ((:file "package")
	       (:file "version"
		:depends-on ("package"))))))


;; cl-hacks.asd ends here
