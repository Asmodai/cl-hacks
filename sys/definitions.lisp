;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; definitions.lisp --- Various methods for defining things
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:48:58 asmodai>
;;; Revision:   8
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    25 Nov 2011 23:44:17
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
;;;{{{ Internal methods:

(defun %re-evaluate-constant (name value test)
  (if (not (boundp name))
      value
      (let ((old (symbol-value name))
            (new value))
        (if (not (constantp name))
            (prog1 new
              (cerror "Trying to redefine the variable as a constant."
                      "~@<~S is an already-bound non-constant ~
                       variable whose value is ~S.~:@>" name old))
            (if (funcall test old new)
                old
                (restart-case
                    (error "~@<~S is an already-defined constant ~
                            whose value ~S is not equal to the ~
                            provided initial value ~S under ~S.~:@>"
                           name old new test)
                    (ignore ()
                      :report "Retain the current value."
                      old)
                    (continue ()
                      :report "Try to redefine the constant."
                      new)))))))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Defining constants and variables:

(defmacro define-constant (name initial-value
                           &key (test ''equal) docstr)
  `(defconstant ,name (%re-evaluate-constant ',name ,initial-value
                                             ,test)
     ,@(when docstr `(,docstr))))

(defmacro defconstant* (sym value &optional docstr)
  `(defconstant ,sym (if (boundp ',sym)
                         (symbol-value ',sym)
                         ,value)
     ,@(when docstr (list docstr))))

(defmacro defcustom (name type init docstr)
  `(progn
     (declaim (type ,type ,name))
     (defvar ,name (the ,type ,init) ,docstr)))

(defmacro defconst (name type init docstr)
  `(progn
     (declaim (type ,type ,name))
     (,(if (subtypep type '(or symbol number character))
           'defconstant
           'defvar)
       ,name (the ,type ,init) ,docstr)))

(defmacro make-typed-array (type init &optional len)
  (if len
      `(make-array ,len :element-type ,type :initial-element ,init)
      `(make-array (length ,init) :element-type ,type
                   :initial-contents ,init)))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Constant predicates and evaluation:

(defun named-constant-p (name &optional env)
  env
  (when (constantp name)
    (return-from named-constant-p
      (values t (symbol-value name))))
  nil)

(defun evaluate-constant (form &optional env)
  (setf form (macroexpand form env))
  (if (atom form)
      (cond ((not (symbolp form))
             form)
            (t
             (multiple-value-bind (constant-p value)
                 (named-constant-p form env)
               (if constant-p
                   value
                   (error "Symbol ~S does not have a value."
                          form)))))
      (case (first form)
        (quote
         (second form))
        (values
         (apply #'values
                (loop for value in (cdr form)
                      collect (evaluate-constant value env))))
        (otherwise
         (error "Constant evaluator called on ~S" form)))))

;;;}}}
;;; ==================================================================

;;; definitions.lisp ends here
