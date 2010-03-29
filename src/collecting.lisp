;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; collecting.lisp --- Collecting lists forwards
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:25:16 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 1989-2000 Tim Bradshaw
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Sep 2009 21:27:12
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

(defmacro collecting (&body forms)
  ;; Collect some random stuff into a list by keeping a tail-pointer
  ;; to it, return the collected list.  No real point in using
  ;; gensyms, although one probably should on principle.
  "Collect things into a list forwards.  Within the body of this macro
the form `(COLLECT THING)' will collect THING into the list returned
by COLLECTING.  Uses a tail pointer -> efficient."
  (let (($resnam$ (gensym)) ($tail$ (gensym)) ($thing$ (gensym)))
    `(let
      (,$resnam$ ,$tail$)
      (macrolet
	  ((collect
	       (thing)
	     ;; Collect returns the thing it's collecting
	     `(let ((,',$thing$ ,thing))
	       (if ,',$resnam$
		   (setf (cdr ,',$tail$)
			 (setf ,',$tail$ (list ,',$thing$)))
		   (setf ,',$resnam$
			 (setf ,',$tail$ (list ,',$thing$))))
	       ,',$thing$)))
	,@forms)
      ,$resnam$)))

(defmacro with-collectors ((&rest collectors) &body forms)
  ;; multiple-collector version of COLLECTING.
  "Collect some things into lists forwards. The names in COLLECTORS
are defined as local macros, which each collect into a separate list.
Returns as many values as there are collectors."
  (let ((cvns (mapcar #'(lambda (c)
			  (make-symbol (concatenate 'string
						    (symbol-name c) "-VAR")))
		      collectors))
	(ctns (mapcar #'(lambda (c)
			  (make-symbol (concatenate 'string
						    (symbol-name c) "-TAIL")))
		      collectors)))
    `(let (,@cvns ,@ctns)
      (macrolet ,(mapcar #'(lambda (cn cvn ctn)
			     `(,cn (v)
			       (let ((vn (make-symbol "V")))
				 `(let ((,vn ,v))
				   (if ,',cvn
				       (setf (cdr ,',ctn)
					     (setf ,',ctn (list ,vn)))
				       (setf ,',cvn 
					     (setf ,',ctn (list ,vn))))
				   ,vn))))
			 collectors cvns ctns)
	,@forms
	(values ,@cvns)))))

;;; collecting.lisp ends here
