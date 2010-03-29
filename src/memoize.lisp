;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; memoize.lisp --- Memoization
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:30:35 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 1995-2000 Tim Bradshaw
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Sep 2009 21:01:25
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
;;; Norvig, pp269-275
;;;
;;; Note that memoized functions are not currently thread-safe, since
;;; calling them can modify the structure holding the memos.
;;;
;;; }}}

#-genera
(in-package #:cl-hacks)

(defvar *memoized-functions* '()
  "Stores an alist of (name table old-def).")

(defun make-memo (fn key test)
  ;; Return wrapper & table
  (declare (type function fn key test))
  (let ((table (make-hash-table :test test)))
    (values
     #'(lambda (&rest args)
	 (declare (dynamic-extent args))
	 (let ((k (funcall key args)))
	   (multiple-value-bind (val found-p)
	       (gethash k table)
	     (if found-p
		 val
		 (setf (gethash k table) (apply fn args)))))))
    table))

(defun memoize-function (fn-name &key (key #'first) (test #'eql))
  "Memoize FN-NAME, a symbol, causing its results to be stashed.
KEY is a function which is given the arglist of FN-NAME, and should
return a key to hash on for memoizing.  TEST is a function which is
the test for the hashtable.

See Norvig pp269-275.

Note this function may not work on self-recursive functions because
the compiler can optimize away self-calls in various ways.

DEF-MEMOIZED-FUNCTION should work for those cases as it is careful to
ensure the function can not be inlined like this."
  (declare (type symbol fn-name)
	   (type function key test))
  (when (not (fboundp fn-name))
    (error "~a is not FBOUND." fn-name))
  (when (assoc fn-name *memoized-functions*)
    (error "~a is already memoized." fn-name))
  (multiple-value-bind (wrapper table)
      (make-memo (symbol-function fn-name) key test)
    (push (list fn-name table (symbol-function fn-name))
	  *memoized-functions*)
    (setf (symbol-function fn-name) wrapper)
    fn-name))

(defun unmemoize-function (fn-name)
  "Remove memoization for FN-NAME."
  (declare (type symbol fn-name))
  (let ((hit (assoc fn-name *memoized-functions*)))
    (when (not hit)
      (error "~a is not memoized." fn-name))
    (setf (symbol-function fn-name) (third hit))
    (setf *memoized-functions* (delete hit *memoized-functions*))
    fn-name))

(defun unmemoize-functions ()
  "Unmemoize all functions"
  (mapcar #'unmemoize-function
	  (mapcar #'car *memoized-functions*)))

(defun clear-memoized-function (fn-name)
  "Clear memoized results for FN-NAME."
  (declare (type symbol fn-name))
  (let ((hit (assoc fn-name *memoized-functions*)))
    (when (not hit)
      (error "~a is not memoized." fn-name))
    (clrhash (second hit))
    fn-name))

(defun clear-memoized-functions ()
  "Clear memoized results for all functions."
  (mapcar #'clear-memoized-function
	  (mapcar #'car *memoized-functions*)))

(defun function-memoized-p (fn-name)
  "Is FN-NAME memoized?"
  (declare (type symbol fn-name))
  (if (assoc fn-name *memoized-functions*)
      t
      nil))

(defmacro def-memoized-function (fnspec args &body body)
  "Define a memoized function.

FNSPEC is either the name of the function, or a list suitable as an
arglist for MEMOIZE-FUNCTION.  ARGS & BODY are passed off to DEFUN.

This will declare FNSPEC NOTINLINE, which may be necessary to prevent
good compilers optimizing away self calls & stuff like that."
  (let* ((normalized-fnspec (etypecase fnspec
			      (symbol (list fnspec))
			      (list fnspec)))
	 (name (car normalized-fnspec)))
    (when (function-memoized-p name)
      (unmemoize-function name))
    `(progn
      ;; ??? is this right?  I want to ensure that the function is
      ;; really called, and avoid bright compilers doing TRO or
      ;; not calling through the SYMBOL-FUNCTION (kind of a strange
      ;; thing to want in general).  I think that a NOTINLINE
      ;; declaration does this.
      (declaim (notinline ,name))
      (defun ,name ,args
	,@body)
      (apply #'memoize-function (list ',(car normalized-fnspec)
				 ,@(cdr normalized-fnspec)))
      ',name)))

(defmacro memoized-labels ((&rest labdefs) &body bod)
  "A version of LABELS that memoizes the local functions.  See
MEMOIZE-FUNCTION and DEF-MEMOIZED-FUNCTION.  If code that uses this is
compiled (either by COMPILE or COMPILE-FILE, then the table of memoized
results will be unique, if interpreted then a new table may be generated for
each use.  The function `names' are generalised in the same way as for 
DEF-MEMOIZED-FUNCTION."
  ;; this is a pretty hairy macro, perhaps unnecessarily so.  It uses
  ;; an interestingly-large amount of the features of CL.  The use of
  ;; LOAD-TIME-VALUE is an attempt to get literal hashtables into the
  ;; compiled code, which seems to be non-portable the obvious way
  ;; (binding them in the macro & then splicing the literal in to the
  ;; expansion). Can MAKE-LOAD-FORM do this better?
  `(labels ,(loop for (fspec fargs . fbod) in labdefs
		  collect
		  (destructuring-bind (fname &key (key '(function first))
					     (test '(function eql)))
		      (if (listp fspec)
			  ;; FSPEC is of the form (NAME :key
			  ;; .. :test ..), where we use the keywords
			  ;; to get the key from the arglist and
			  ;; decide what test to use for the
			  ;; hashtable.
			  fspec
			  (list fspec :key '(function first) 
				:test '(function eql)))
		    (let ((htn (make-symbol "HT"))	;hashtable name
			  (kn (make-symbol "K"))	;key from arglist name
			  (vn (make-symbol "V"))	;value found name
			  (fpn (make-symbol "FP"))	;foundp name
			  (argsn (make-symbol "ARGS")))	;args name
		      ;; here's the definition clause in the LABELS:
		      ;; note we have to generalise rthe args to an
		      ;; &REST, but hopefully the DYNAMIC-EXTENT
		      ;; avoids too much lossage.
		      `(,fname (&rest ,argsn)
			(declare (dynamic-extent ,argsn)	;stop consing
			 (notinline ,fname))	;stop TRO (?)
			;; this use of LOAD-TIME-VALUE should ensure
			;; that the hashtable is unique in compiled
			;; code.  This has kind of interesting
			;; effects, as it's shared amongst seperate
			;; closures that you might return, so use of
			;; one can speed up another!
			(let ((,htn (load-time-value (make-hash-table
						      :test ,test)))
			      (,kn (funcall ,key ,argsn)))
			  (multiple-value-bind (,vn ,fpn)
			      (gethash ,kn ,htn)
			    (if ,fpn
				,vn		;found in table: return value
				;; didn't find it: compute value
				(setf (gethash ,kn ,htn)
				      (apply #'(lambda ,fargs
						 ,@fbod)
					     ,argsn)))))))))
    ,@bod))

;;; indentation for zmacs
#+genera
(pushnew 'memoized-labels zwei:*definition-list-functions*)

;;; memoize.lisp ends here

