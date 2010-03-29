;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; math.lisp --- Various math stuff
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:30:21 asmodai>
;;; Revision:   13
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2002 Keven M. Rosenberg
;;; Copyright (c) 2002 Tim Bradshaw
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Tue Sep  1 19:00:00 2009
;;; Keywords:   Common Lisp CLOS Hacks
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

(defun deriv (f dx)
  #'(lambda (x)
      (/ (- (funcall f (+ x dx)) (funcall f x))
	 dx)))

(defun sin^ (x)
  (funcall (deriv #'sin 1d-8) x))

(defmacro ensure-integer (obj)
  `(if (stringp ,obj)
       (parse-integer ,obj)
       ,obj))

(defun histogram (v n-bins &key min max)
  (declare (fixnum n-bins))
  (when (listp v)
    (setq v (coerce v 'vector)))
  (when (zerop (length v))
    (return-from histogram (values nil nil nil )))
  (let ((n (length v))
	(bins (make-array n-bins
			  :element-type 'integer
			  :initial-element 0))
	found-min found-max)
    (declare (fixnum n))
    (unless (and min max)
      (setq found-min (aref v 0)
	    found-max (aref v 0))
      (loop for i fixnum from 1 to (1- n)
	    do (let ((x (aref v i)))
		 (cond ((> x found-max)
			(setq found-max x))
		       ((< x found-min)
			(setq found-min x)))))
      (unless min
	(setq min found-min))
      (unless max
	(setq max found-max)))
    (let ((width (/ (- max min) n-bins)))
      (setq width (+ width (* double-float-epsilon width)))
      (dotimes (i n)
	(let ((bin (nth-value 0 (truncate (- (aref v i) min) width))))
	  (declare (fixnum bin))
	  (when (and (not (minusp bin))
		     (< bin n-bins))
	    (incf (aref bins bin))))))
    (values bins min max)))

(defun fixnum-width ()
  (nth-value 0 (truncate (+ (/ (log (1+ most-positive-fixnum)) (log 2)) .5))))

(defun scaled-epsilon (float &optional (operation '+))
  (multiple-value-bind (significand exponent)
      (decode-float float)
    (multiple-value-bind (1.0-significand 1.0-exponent)
	(decode-float (float 1.0 float))
      (if (and (eq operation '-)
	       (= significand 1.0-significand))
	  (scale-float (typecase float
			 (short-float short-float-negative-epsilon)
			 (single-float single-float-negative-epsilon)
			 (double-float double-float-negative-epsilon)
			 (long-float long-float-negative-epsilon))
		       (- exponent 1.0-exponent))
	  (scale-float (typecase float
			 (short-float short-float-epsilon)
			 (single-float single-float-epsilon)
			 (double-float double-float-epsilon)
			 (long-float long-float-epsilon))
		       (- exponent 1.0-exponent))))))

(defun sinc (x)
  (if (zerop x)
      1d0
      (let ((x (coerce x 'double-float)))
	(/ (sin x) x))))

(defun numbers-within-percentage (a b percent)
  (let ((abs-diff (* 0.01 percent 0.5 (+ (abs a) (abs b)))))
    (< (abs (- a b)) abs-diff)))

(defun print-float-units (val unit &optional (stream nil))
  (cond ((< val 1d-6) (format stream "~,2,9f nano~a" val unit))
	((< val 1d-3) (format stream "~,2,6f micro~a" val unit))
	((< val 1)    (format stream "~,2,3f mili~a" val unit))
	((> val 1d9)  (format stream "~,2,-9f giga~a" val unit))
	((> val 1d6)  (format stream "~,2,-6f mega~a" val unit))
	((> val 1d3)  (format stream "~,2,-3f kilo~a" val unit))
	(t            (format stream "~,2f ~a" val unit))))

;;; math.lisp ends here
