;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; mp.lisp --- Multiprocessing support
;;;
;;; Time-stamp: <Monday Dec  5, 2011 05:07:34 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    18 Oct 2011 09:54:58
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
(in-package #:cl-hacks)

(defun mp-make-lock (&optional (name "Anonymous"))
  #+allegro
  (mp:make-process-lock :name name)
  #+sbcl
  (sb-thread:make-mutex :name name)
  #+(and cmu x86)
  (mp:make-lock name)
  #+(and cmu (not x86))
  (declare (ignore name))
  #+openmcl
  (ccl:make-lock name)
  #+lispworks
  (mp:make-lock :name name)
  #-(or allegro sbcl cmu openmcl lispworks)
  (error "Missing mutex locking primitive for your compiler.
Please provide one in ~A."
         *load-pathname*))
         
(defmacro mp-with-lock-held ((lock) &body body)
  #+allegro
  `(mp:with-process-lock (,lock)
     ,@body)
  #+sbcl
  `(sb-thread:with-mutex (,lock)
     ,@body)
  #+cmu
  `(mp:with-lock-held (,lock)
     ,@body)
  #+openmcl
  `(ccl:with-lock-grabbed (,lock)
     ,@body)
  #+lispworks
  `(mp:with-lock (,lock)
     ,@body)
  #-(or allegro sbcl cmu openmcl lispworks)
  (error "Missing `with-lock-held' macro for your compiler.
Please provide one in ~A."
         *load-pathname*))

(defmacro mp-with-recursive-lock-held ((lock) &body body)
  #+allegro
  `(mp:with-process-lock (,lock)
     ,@body)
  #+sbcl
  `(sb-thread:with-recursive-lock (,lock)
     ,@body)
  #+cmu
  `(mp:with-lock-held (,lock)
     ,@body)
  #+openmcl
  `(ccl:with-lock-grabbed (,lock)
     ,@body)
  #+lispworks
  `(mp:with-lock (,lock)
     ,@body)
  #-(or allegro sbcl cmu openmcl lispworks)
  (error "Missing `with-recursive-lock-held' for your compiler.
Please provide one in ~A."
         *load-pathname*))

(defun make-process (fn &key name)
  #+sbcl
  (sb-thread:make-thread fn :name name)
  #+cmu
  (mp:make-process fn :name name)
  #+openmcl
  (ccl:process-run-function name fn)
  #-(or sbcl cmu openmcl)
  (error "Missing `make-process' for your compiler.
Please provide one in ~A."
         *load-pathname*))

(defun destroy-process (process)
  #+sbcl
  (sb-thread:destroy-thread process)
  #+cmu
  (mp:destroy-process process)
  #+openmcl
  (ccl:process-kill process)
  #-(or sbcl cmu openmcl)
  (error "Missing `destroy-process' for your compiler.
Please provide one in ~A."
         *load-pathname*))

(defun process-active-p (process)
  #+sbcl
  (sb-thread:thread-alive-p process)
  #+cmu
  (mp:process-active-p process)
  #+openmcl
  (ccl::process-active-p process)
  #-(or sbcl cmu openmcl)
  (error "Missing `process-active-p' for your compiler.
Please provide one in ~A."
         *load-pathname*))

;;; mp.lisp ends here
