;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; macros.lisp --- Various macros
;;;
;;; Time-stamp: <Friday Dec 14, 2012 05:40:25 asmodai>
;;; Revision:   9
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    26 Nov 2011 00:39:57
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

;;; ==================================================================
;;;{{{ Gensym macros:

(defmacro with-gensyms (names &body forms)
  `(let ,(mapcar (lambda (name)
                   (multiple-value-bind (symbol string)
                       (etypecase name
                         (symbol
                          (values name (symbol-name name)))
                         ((cons symbol (cons string-designator null))
                          (values (first name)
                                  (string (second name)))))
                     `(,symbol (gensym ,string))))
                 names)
     ,@forms))

#-lispworks
(defmacro with-unique-names (names &body forms)
  `(with-gensyms ,names ,@forms))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Evaluation macros:

;;; Genera already has ONCE-ONLY.
#-genera
(defmacro once-only (variable-list &body body &aux environment-var)
  (declare (ignore environment-var))
  (let ((gensyms (make-gensym-list (length variable-list) "ONCE-ONLY"))
        (names-and-forms (mapcar (lambda (spec)
                                   (etypecase spec
                                     (list
                                      (destructuring-bind (name form)
                                          spec
                                        (cons name form)))
                                     (symbol
                                      (cons spec spec))))
                                 variable-list)))
    ;; Bind in user-macro
    `(let ,(mapcar (lambda (g n)
                     (list g `(gensym ,(string (car n)))))
                   gensyms
                   names-and-forms)
       ;; Bind in final expression
       `(let (,,@(mapcar (lambda (g n)
                           ``(,,g ,,(cdr n)))
                   gensyms
                   names-and-forms))
          ;; Bind in user-macro
          ,(let ,(mapcar (lambda (n g)
                           (list (car n) g))
                         names-and-forms
                         gensyms)
             ,@body)))))

(defmacro with-ignore-errors (&rest forms)
  `(progn
     ,@(mapcar (lambda (x)
                 (list 'ignore-errors x))
               forms)))

;;;}}}
;;; ==================================================================

;;; macros.lisp ends here
