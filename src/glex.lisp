;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; glex.lisp --- Global lexical variables
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:43:26 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;; Coptrigh (c) Tim Bradshaw
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 17:06:39
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
(in-package #:cl-hacks)

;;; This makes the symbol-macro easier to write (want an error on unbound)
(declaim (inline glex-value (setf glex-value)))

(define-condition unbound-global-lexical (unbound-variable)
  ;; I feel guilty about using UNNOUND-VARIABLE because it isn't.
  ())

;;; This just uses property lists behind the scenes now
(defun glex-value (sym)
  (multiple-value-bind (boundp val) (get-properties (symbol-plist sym) 
                                                    '(glex-value))
    (unless boundp
      (error 'unbound-global-lexical :name sym))
    val))

(defun (setf glex-value) (new sym)
  (setf (get sym 'glex-value) new))

(defmacro defglex (x &optional (value nil valuep) (documentation nil docp))
  ;; DEFGLEX is like DEFVAR, (not DEFPARAMETER), but for global lexicals
  `(progn
     ,@(if valuep
           `((unless (get-properties (symbol-plist ',x) '(glex-value))
               (setf (glex-value ',x) ,value)))
           '())
     ;; This must be known at compile time so users can be compiled.
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (define-symbol-macro ,x (glex-value ',x)))
     ,@(if docp
           `((setf (documentation ',x 'variable) ',documentation))
           '())
     ',x))

(defmacro defglpar (x &optional (value nil valuep) (documentation nil docp))
  ;; DEFGLPAR is like DEFPARAMETER, but for global lexicals
  `(progn
     ,@(if valuep
           `((setf (glex-value ',x) ,value))
           '())
     ;; This must be known at compile time so users can be compiled.
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (define-symbol-macro ,x (glex-value ',x)))
     ,@(if docp
           `((setf (documentation ',x 'variable) ',documentation))
           '())
     ',x))

;;; glex.lisp ends here
