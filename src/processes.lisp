;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; processes.lisp --- Multi-processing functions.
;;;
;;; Time-stamp: <Tuesday Mar 30, 2010 08:12:49 asmodai>
;;; Revision:   5
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2002 Keven M. Rosenberg
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

(in-package #:cl-hacks)

#+genera
(error "The functions contained in PROCESSES.LISP can clobber Symbolics
functions.  Please do not load this file.")

(defun make-process (name func)
  #+allegro (mp:process-run-function name func)
  #+cmu (mp:make-process func :name name)
  #+lispworks (mp:process-run-function name nil func)
  #+sb-thread (sb-thread:make-thread func :name name)
  #+openmcl (ccl:process-run-function name func)
  #-(or allegro cmu lispworks sb-thread openmcl)
  (funcall func))

(defun destroy-process (process)
  #+cmu (mp:destroy-process process)
  #+allegro (mp:process-kill process)
  #+sb-thread (sb-thread:destroy-thread process)
  #+lispworks (mp:process-kill process)
  #+openmcl (ccl:process-kill process))

(defun make-lock (name)
  #+allegro (mp:make-process-lock :name name)
  #+cmu (mp:make-lock name)
  #+lispworks (mp:make-lock :name name)
  #+sb-thread (sb-thread:make-mutex :name name)
  #+openmcl (ccl:make-lock name))

(defmacro with-lock-held ((lock) &body body)
  #+allegro
  `(mp:with-process-lock (,lock) ,@body)
  #+cmu
  `(mp:with-lock-held (,lock) ,@body)
  #+lispworks
  `(mp:with-lock (,lock) ,@body)
  #+sb-thread
  `(sb-thread:with-recursive-lock (,lock) ,@body)
  #+openmcl
  `(ccl:with-lock-grabbed (,lock) ,@body)
  #-(or allegro cmu lispworks sb-thread openmcl)
  `(progn ,@body))

(defmacro with-timeout ((seconds) &body body)
  #+allegro
  `(mp:with-timeout (,seconds) ,@body)
  #+cmu
  `(mp:with-timeout (,seconds) ,@body)
  #+sb-thread
  `(sb-ext:with-timeout ,seconds ,@body)
  #+openmcl
  `(ccl:process-wait-with-timeout "waiting"
                                 (* ,seconds ccl:*ticks-per-second*)
                                 #'(lambda ()
                                     ,@body) nil)
  #-(or allegro cmu sb-thread openmcl genera)
  `(progn ,@body))

(defun process-sleep (n)
  #+allegro (mp:process-sleep n)
  #-(and allegro (not genera)) (sleep n))

;;; processes.lisp ends here
