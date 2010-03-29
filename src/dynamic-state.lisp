;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; dynamic-state.lisp --- Dynamic state access
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:26:40 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2001 Tim Bradshaw
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Sep 2009 21:30:11
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

#-genera
(in-package #:cl-HACKS)

(defmacro define-dynamic-state ((binder accessor) &rest all-specials)
  ;; define a binder, BINDER, and accessor, ACCESSOR for some dynamic
  ;; state variables.  The legal variables must come from
  ;; ALL-SPECIALS.
  `(progn
    (defmacro ,binder (bindings &body body)
      (let ((varnames
      ;; Establish a dynamic state: binding specs like LET.
	     (mapcan #'(lambda (b)
			 (typecase b
			   (symbol 
			    (unless (member b ',all-specials)
			      (error "~S is not a valid dynamic state variable for ~S" 
				     b ',binder))
			    (list b))
			   (cons
			    (unless (and (= (length b) 2)
					 (symbolp (first b)))
			      (error "~S is not a valid binding specification" b)) 
			    (unless (member (first b) ',all-specials)
			      (error "~S is not a valid dynamic state variable for ~S"
				     (first b) ',binder))
			    (list (first b)))
			   (t
			    (error "~S is not a valid binding specification" b))))
		     bindings)))
	;; try and generate slightly reasonable-looking code.
	(if (not (null varnames))
	    `(let ,bindings
	      (declare (special ,@varnames))
	      ,@body)
	    `(locally
	      ,@body))))
    
    (defmacro ,accessor (varnames &body body) 
      ;; get access to a dynamic state -- VARNAMES is list of
      ;; variables we want to see.
      (dolist (v varnames)
	(unless (symbolp v)
	  (error "~S is not a valid binding specification" v))
	(unless (member v ',all-specials)
	  (error "~S is not a valid dynamic state variable for ~S"
		 v ',accessor)))
      ;; try and generate slightly reasonable-looking code.
      (if (not (null varnames))
	  `(locally
	    (declare (special ,@varnames))
	    ,@body)
	  `(locally
	    ,@body)))
    '(,binder ,accessor)))

#||
(define-dynamic-state (with-dynamic-state with-dynamic-state-access)
    error-code result) 

(defun foo (x)
  (with-dynamic-state ((result x))
    (bar)
    (values (let ((result 10))
	      ;; This RESULT is *lexical*, so this closes over it
	      #'(lambda (x)
		  (cons x result)))
	    result)))

(defun bar ()
  (let ((result 12))
    ;; This closure closes over the *lexical* RESULT we have here.
    (henry #'(lambda (x)
	       (cons x result)))))

(defun henry (fn)
  (with-dynamic-state-access (result)
    (setf result fn)))
||#

;;; dynamic-state.lisp ends here

