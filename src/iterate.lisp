;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; iterate.lisp --- Applicative iteration
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:29:43 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 1997-2000 Tim Bradshaw
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Sep 2009 20:36:19
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
(in-package #:cl-hacks)

(defconstant +tr-implementation-p+
  #+genera nil           ; Genera does not do TRO
  ;; Assume true: even though this may not be safe, you'll soon
  ;; realise at runtime
  #-genera t
  "Can this implementation perform tail-call optimisation?

Specifically, can it optimise (LABELS ((X (...) ... (X ...) ...))),
where the call to X is in tail position, given reasonable constaints
(no special bindings, in the case of CMU for instance.")

(defmacro iterate (name bindings &body body)
  "Scheme-style named-LET, with hacks.

For implementations which can do TRO, this compiles into LABELS and
recursive calls, which is fully general.

For implementations which can not, *if* the name contains the word
`LOOP' (any case), then this compiles into something dreadful using
BLOCK and RETURN-FROM, which is fast for looping but can't be used
recursively.  If the name does *not contain `LOOP', it compiles into
LABELS as above.

Notes: bindings are sequential not parallel (because LABELS is), so
this is like LET* not LET.  The local function defined should be
considered to have dynamic extent."
  (cond (+tr-implementation-p+
	 ;; LABELS is free
	 `(iterate/labels ,name ,bindings ,@body))
	((search "LOOP" (string-upcase (symbol-name name)))
	 ;; Not TR, and we asked for a loop
	 `(iterate/tag ,name ,bindings ,@body))
	(t
	 ;; Not TR, not loop
	 `(iterate/labels ,name ,bindings ,@body))))

(defmacro iterate/labels (name bindings &body body)
  ;; This one is the fully-fleged variant: not that this is like LET*,
  ;; not LET.
  (let ((argnames ())
	(argvals ()))
    (labels ((grind-bindings (tail)
	       (if (not (null tail))
		   (etypecase (car tail)
		     (symbol
		      (grind-bindings (cdr tail))
		      (push (car tail) argnames)
		      (push nil argvals))
		     (list
		      (grind-bindings (cdr tail))
		      (push (car (car tail)) argnames)
		      (push (cadr (car tail)) argvals))))))
      (grind-bindings bindings)
      `(labels ((,name ,argnames
		 #+genera (declare (sys:downward-function))
		 ,@body))
	(,name ,@argvals)))))

(defmacro iterate/tag (tag bindings &body body)
  ;; This is a hacky one!  It really, really is a hack, believe me.
  ;; Note that the bindings are in sequence, not pll -- because
  ;; ITERATE is (accidentally), and I want this to be the same.  I
  ;; presume that compilers get code that is as good for PROG* as PROG
  ;; if there are no dependencies.
  (let ((argnames (mapcar #'(lambda (binding)
			      (etypecase binding
				(symbol binding)
				(cons (car binding))))
			  bindings)))
    ;; This used to use PROG*, but it's not clear if that really
    ;; portably allows an initial symbol as a name for the block.
    ;; This does (and gets the same code).
    `(block ,tag
      (let* ,bindings
	(tagbody
	   ,tag
	   (macrolet ((,tag (&rest args)
			`(progn
			  (setf ,@(mapcan #'(lambda (name val)
					      (list name val))
					  ',argnames args))
			  (go ,',tag))))
	     ;; It's a pain that all the GO-containing forms need to
	     ;; be RETURNed from explicity rather than returning the
	     ;; last value.  On the SB this means the body of the
	     ;; loop is one instruction bigger than DOTIMES, because
	     ;; the RETURN compiles into a conditional branch (see
	     ;; below for samples).  Anyway that's why this slightly
	     ;; obscure thing is done.
	     (return-from ,tag (progn ,@body))))))))

;;; iterate.lisp ends here

