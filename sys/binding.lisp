;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; binding.lisp --- Various binding functions and macros
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:48:04 asmodai>
;;; Revision:   5
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    26 Nov 2011 01:09:18
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

(defmacro if-let (bindings &body (then-form &optional else-form))
  (let* ((binding-list (if (and (consp bindings)
                                (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           ,then-form
           ,else-form))))

(defmacro when-let (bindings &body forms)
  (let* ((binding-list (if (and (consp bindings)
                                (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))

(defmacro when-let* (bindings &body forms)
  (let ((binding-list (if (and (consp bindings)
                               (symbolp (car bindings)))
                          (list bindings)
                          bindings)))
    (labels ((bind (bindings forms)
               (if bindings
                   `((let (,(car bindings))
                       (when ,(caar bindings)
                         ,@(bind (cdr bindings) forms))))
                   forms)))
      `(let (,(car binding-list))
         (when ,(caar binding-list)
           ,@(bind (cdr binding-list) forms))))))

;;; binding.lisp ends here

