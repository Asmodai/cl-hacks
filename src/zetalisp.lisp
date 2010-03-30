;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; zetalisp.lisp --- Nice Zetalisp stuff
;;;
;;; Time-stamp: <Wednesday Mar 31, 2010 00:27:13 asmodai>
;;; Revision:   5
;;;
;;; Copyright (c) 2010 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    30 Mar 2010 14:25:29
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
;;;
;;; {{{ Commentary:
;;;
;;; }}}

#+genera
(error "These functions are already provided by Zetalisp.")

(in-package #:cl-hacks)

;;; ------------------------------------------------------------------
;;; {{{ NAMED-LAMBDA:

;; Gah, this is a kludge.
(defmacro named-lambda (name lambda-list &body body)
  (declare (ignore name))
  `(lambda ,lambda-list ,@body))

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ STANDARDIZE-FUNCTION-SPEC:

(defun standardize-function-spec (fspec &optional (error-p t))
  (and (listp fspec)
       (= (length fspec) 2)
       (symbolp (car fspec)))
  (unless (validate-function-spec fspec)
    (if error-p
	(error "~S is not a valid function spec" fspec)
	(return-from standardize-function-spec nil)))
  fspec)

(defun validate-function-spec (fspec &optional nil-allowed &aux handler)
  (cond ((null fspec) nil-allowed)
	((symbolp fspec) t)
	((atom fspec) nil)))

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ DEFSELECT:

(defmacro defselect (fspec &body methods)
  "Define a function named FSPEC which dispatches on its first argument to
find a method.  Each element of METHODS is a method for one or several
possible first arguments.  Syntax:

   (DEFSELECT fspec           (DEFSELECT (fspec default-handler no-which-operations)
     (operation (args ...)      (operation (args ...)
       body ...)                  body ...)
     (operation (args ...)      (operation (args ...)
       body ...))                 body ...))

FSPEC is the name of the function to be defined.  OPERATION is a keyword or a
list of keywords that names the operations to be handled by the clause.  ARGS
is a lambda-list for the clause;  it should not include an argument for the
operation.  BODY is the body of the function for the clause.  When FSPEC is
called, it will choose a clause based on the first argument, bind its
parameters to the remaining arguments, and run the body, returning its
result.

A clause may instead look like (OPERATION . SYMBOL), in which SYMBOL is the
name of a function that will be called for the OPERATION.  It will be given
all the arguments, including the operation symbol itself, unlike the body of
a normal clause.

DEFAULT-HANDLER is optional;  it is a function which is called if the first
argument is an unknown operation.  If unsupplied or NIL, an unknown operation
will cause a continuable error.  NO-WHICH-OPERATIONS is also optional;  if
non-NIL, the automatically-generated clauses for :WHICH-OPERATIONS,
:OPERATION-HANDLED-P, and :SEND-IF-HANDLES are suppressed."
  (let* ((default-handler (if (consp fspec) (second fspec) nil))
         (no-which-operations (if (consp fspec) (third fspec) nil))
	 (fspec (standardize-function-spec (if (consp fspec) (first fspec) fspec)))
         (operation-list nil)
         (clauses-list nil))
    (loop for (key . method-body) in methods
          doing (if (consp key)
                    (setq operation-list (revappend key operation-list))
                    (push key operation-list))
          doing (if (symbolp method-body)
                    (push `(,key (apply #',method-body op args)) clauses-list)
                    (push `(,key (apply (lambda ,@method-body) args))
                          clauses-list)))
    (cond (no-which-operations
           (setq operation-list (nreverse operation-list)))
          (:else
           (setq clauses-list
                 (append `((:which-operations
                            (apply (lambda (&rest ignore)
				     ',operation-list) args))
                           (:operation-handled-p
                            (apply (lambda (op &rest ignore)
				     (not (null (member op ',operation-list :test #'eq))))
                                   args))
                           (:send-if-handles
                            (apply (lambda (op &rest to-send)
				     (when (member op ',operation-list :test #'eq)
				       (apply (function ,fspec) op to-send)))
                                   args)))
                         clauses-list))))
    (setq clauses-list (nreverse clauses-list))
    `(defun ,fspec (op &rest args)
       ,(if default-handler
            `(case op
               ,@clauses-list
               (otherwise (apply #',default-handler op args)))
	    `(ccase op
	       ,@clauses-list)))))

;;; }}}
;;; ------------------------------------------------------------------


;;; zetalisp.lisp ends here

