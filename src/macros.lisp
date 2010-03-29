;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; macros.lisp --- Several macro hacks.
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:30:04 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2002 Keven M. Rosenberg
;;; Copyright (c) Tim Bradshaw
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Tue Sep  1 19:00:00 2009
;;; Keywords:   Common Lisp Macro Hacks
;;; URL:        http://unixware.kicks-ass.org/
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

#-genera
(in-package #:cl-hacks)

;;; ===================================================================
;;; {{{ LET-WHEN, LET-IF:                           (Source: KMRCL)

(defmacro let-when ((var test-form) &body body)
  `(let ((,var ,test-form))
     (when ,var ,@body)))
 
(defmacro let-if ((var test-form) then &optional else)
  `(let ((,var ,test-form))
     (if ,var ,then ,else)))

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ AIF, AWHEN, AWHILE, AAND, ACOND, ALAMBDA:   (Source: KMRCL)

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args)
	 t)
	((null (cdr args))
	 (car args))
	(t
	 `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
	    (sym (gensym)))
	`(let ((,sym ,(car cl1)))
	   (if ,sym
	       (let ((it ,sym))
		 ,@(cdr cl1))
	       (acond ,@(cdr clauses)))))))

(defmacro alambda (params &body body)
  `(Labels ((self ,params ,@body))
     #'self))

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ AIF2, AWHEN2, AWHILE2, ACOND2:              (Source: KMRCL)

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win)
	 ,test
       (if (or it ,win)
	   ,then
	   ,else))))

(defmacro awhen2 (test &body body)
  `(aif2 ,test
	 (progn ,@body)))

(defmacro awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
	 (aif2 ,test
	       (progn ,@body)
	       (setq ,flag nil))))))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
	    (val (gensym))
	    (win (gensym)))
	`(multiple-value-bind (,val ,win)
	     ,(car cl1)
	   (if (or ,val ,win)
	       (let ((it ,val))
		 ,@(cdr cl1))
	       (acond2 ,@(cdr clauses)))))))

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ UNTIL, WHILE, LOOP-FOR:                     (Source: KMRCL)

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;; this used to be called FOR, but that would conflict with TFB's
;; lovely FOR.
(defmacro loop-for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ WITH-EACH...:                               (Source: KMRCL)

(defmacro with-each-stream-line ((var stream) &body body)
  (let ((eof (gensym))
	(eof-value (gensym))
	(strm (gensym)))
    `(let ((,strm ,stream)
	   (,eof ',eof-value))
       (do ((,var (read-line ,strm nil ,eof) (read-line ,strm nil ,eof)))
	   ((eql ,var ,eof))
	 ,@body))))

(defmacro with-each-file-line ((var file) &body body)
  (let ((stream (gensym)))
    `(with-open-file (,stream ,file :direction :input)
       (with-each-stream-line (,var ,stream)
	 ,@body))))

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ IN, MEAN:                                   (Source: KMRCL)

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
		     choices)))))

(defmacro mean (&rest args)
  `(/ (+ ,@args) ,(length args)))

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ WITH...:                                    (Source: KMRCL)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (x) `(,x (gensym))) syms)
     ,@body))

(defmacro with-ignore-errors (&rest forms)
  `(progn
     ,@(mapcar
	 #'(lambda (x) (list 'ignore-errors x))
	 forms)))

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ Timing functions:                           (Source: KMRCL)

(defmacro time-seconds (&body body)
  (let ((t1 (gensym)))
    `(let ((,t1 (get-internal-real-time)))
       (values
	 (progn ,@body)
	 (coerce (/ (- (get-internal-real-time) ,t1)
		    internal-time-units-per-second)
		 'double-float)))))

(defmacro time-iterations (n &body body)
  (let ((i (gensym))
	(count (gensym)))
    `(progn
       (let ((,count ,n))
	 (format t "~&Test with ~d iterations: ~w" ,count (quote ,body))
	 (let ((t1 (get-internal-real-time)))
	   (dotimes ,i ,count)
	   ,@body)
	 (let* ((t2 (get-internal-real-time))
		(secs (coerse (/ (- t2 t1)
				 internal-time-units-per-second)
			      'double-float)))
	   (format t "~&Total time: ")
	   (print-seconds secs)
	   (format t ", time per iteration: ")
	   (print-seconds (coerce (/ secs ,n) 'double-float)))))))

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ Global lexical variables:            (Source: Tim Bradshaw)

(declaim (inline glex-value (setf glex-value)))

(define-condition unbound-global-lexical (unbound-variable)
  ;; I feel guilty about using UNBOUND-VARIABLE, because it isn't.
  ())

(defun glex-value (sym)
  (multiple-value-bind (boundp val)
      (get-properties (symbol-plist sym) '(glex-value))
    (unless boundp
      (error 'unbound-global-lexical :name sym))
    val))

(defun (setf glex-value) (new sym)
  (setf (get sym 'glex-value) new))

(defmacro defglex (x &optional (value nil valuep) (docstr nil docp))
  "DEFGLEX is like DEFVAR, not DEFPARAMETER, but for global lexicals."
  `(progn
    ,@(if valuep
	  `((unless (get-properties (symbol-plist ',x) '(glex-value))
	      (setf (glex-value ',x) ,value)))
	  '())
    ;; This must be known at compile time so users can be compiled.
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (define-symbol-macro ,x (glex-value ',x)))
    ,@(if docp
	  `((setf (documentation ',x 'variable) ',docstr))
	  '())
    ',x))

(defmacro defglpar (x &optional (value nil valuep) (docstr nil docp))
  "DEFGLPAR is like DEFPARAMETER, but for global lexicals."
  `(progn
    ,@(if valuep
	  `((setf (glex-value ',x) ,value))
	  '())
    ;; This must be known at compile time so users can be compiled.
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (define-symbol-macro ,x (glex-value ',x)))
    ,@(if docp
	  `((setf (documentation ',x 'variable) ',docstr))
	  '())
    ',x))

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ DEF-CACHED-...:                             (Source: KMRCL)

(defmacro def-cached-vector (name element-type)
  (let ((get-name (concat-symbol "get-" name "-vector"))
	(release-name (concat-symbol "release-" name "-vector"))
	(table-name (concat-symbol "*cached-" name "-table*"))
	(lock-name (concat-symbol "*cached-" name "-lock*")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,table-name (make-hash-table :test 'equal))
       (defvar ,lock-name (cl-hacks::make-lock ,name))
       (defun ,get-name (size)
	 (cl-hacks::with-lock-held (,lock-name)
	   (let ((buffers (gethash (cons size ,element-type) ,table-name) buffers)
		 buffer)
	     (make-array size :element-type ,element-type))))
       (defun ,release-name (buffer)
	 (cl-hacks::with-lock-held (,lock-name)
	   (let ((buffers (gethash (cons (array-total-size buffer)
					 ,element-type)
				   ,table-name)))
	     (setf (gethash (cons (array-total-size buffer)
				  ,element-type) ,table-name)
		   (cons buffer buffers))))))))

(defmacro def-cached-instance (name)
  (let* ((new-name (concat-symbol "new-" name "-instance"))
	 (release-name (concat-symbol "release-" name "-instance"))
	 (cache-name (concat-symbol "*cached-" name "-instance-table*"))
	 (lock-name (concat-symbol "*cached-" name "-instance-lock*")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,cache-name nil)
       (defvar ,lock-name (cl-hacks::make-lock ',name))
       (defun ,new-name ()
	 (cl-hacks::with-lock-held (,lock-name)
	   (if ,cache-name
	       (pop ,cache-name)
	       (make-instance ',name))))
       (defun ,release-name (instance)
	 (cl-hacks::with-lock-held (,lock-name)
	   (push instance ,cache-name))))))

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ Macro pretty-printing:                      (Source: KMRCL)

(defmacro pprint-macro (expr)
  "Pretty-print a macro."
  `(pprint (macroexpand ',expr)))

(defmacro pprint-macro-1 (expr)
  "Pretty-print a macro using MACROEXPAND-1."
  `(pprint (macroexpand-1 ',expr)))

(defmacro ppmx (form)
  "Pretty-print the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
	  (exp (macroexpand exp1))
	  (*print-circle* nil))
     (cond ((equal exp exp1)
	    (format t "~&Macro expansion:")
	    (pprint exp))
	   (t
	    (format t "~&First step of expansion:")
	    (pprint exp)))
     (format t "~%~%")
     (values)))

;;; }}}
;;; ===================================================================

(defmacro defconstant* (sym value &optional doc)
  "Ensure VALUE is evaluated once."
  `(defconstant ,sym (if (boundp ',sym)
			 (symbol-value ',sym)
			 ,value)
     ,@(when doc (list doc))))

(defmacro defvar-unbound (sym &optional (doc ""))
  "DEFVAR with a documentation string."
  `(progn
     (defvar ,sym)
     (setf (documentation ',sym 'variable) ,doc)))

;; macros.lisp ends here
