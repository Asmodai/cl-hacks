;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; looping.lisp --- Various looping macros
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:51:53 asmodai>
;;; Revision:   5
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    26 Nov 2011 01:14:02
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
(in-package #:cl-hacks-internals)

(defmacro til (test-form &body body)
  `(do ()
       (,test-form)
     ,@body))

(defmacro while (test-form &body body)
  `(do ()
       ((not ,test-form))
     ,@body))

(defmacro loop-for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

;;; looping.lisp ends here
