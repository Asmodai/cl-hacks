;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; wrapping-standard.lisp --- Wrapping standard method combination
;;;
;;; Time-stamp: <Tuesday Sep 22, 2009 01:22:46 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2001 Tim Bradshaw
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Sep 2009 21:32:35
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

(define-method-combination wrapping-standard ()
  ;; Like standard but WRAPPING methods get called in
  ;; *most-specific-last* order, and before and after any other methods
  ;; The complete order is then:
  ;;
  ;; least specific wrapping method
  ;;  ... call-next-method ...
  ;;  most specific around method 
  ;;   ... call-next-method ...
  ;;   most specific before method ... least specific before method
  ;;    most specific primary method 
  ;;     [... call-next-method ... other primary methods ...]
  ;;   least specific after method ... most specific after method
  ;;  rest of most specific around method
  ;; rest of least specific wrapping method
  ;;
  ((around (:around))
   (wrapping (:wrapping) :order :most-specific-last)
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let* ((form (if (or before after (rest primary))
                     `(multiple-value-prog1
		       (progn ,@(call-methods before)
			      (call-method ,(first primary)
					   ,(rest primary)))
		       ,@(call-methods (reverse after)))
		     `(call-method ,(first primary))))
           (around-form (if around
                            `(call-method ,(first around)
			      (,@(rest around)
			       (make-method ,form)))
			    form)))
      (if wrapping 
          `(call-method ,(first wrapping)
	    (,@(rest wrapping)
	     (make-method ,around-form)))
	  around-form))))

#||
(defgeneric complicated (x &key cache recompute)
  (:method-combination wrapping-standard)
  (:method :wrapping (x &key (cache t) (recompute nil))
	   (call-next-method x :cache cache :recompute recompute)))
||#

;;; wrapping-standard.lisp ends here

