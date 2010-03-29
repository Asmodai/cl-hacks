;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; datetime.lisp --- Time and date functions
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:26:33 asmodai>
;;; Revision:   16
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2002 Keven M. Rosenberg
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

(defun pretty-date (year month day &optional (hour 12) (m 0) (s 0))
  (multiple-value-bind (sec min  hr dy mn yr wkday)
      (decode-universal-time
	(encode-universal-time s m hour day month year))
    (values (elt '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
		   "Saturday" "Sunday")
		 wkday)
	    (elt '("January" "February" "March" "April" "May" "June"
		   "July" "August" "September" "October" "November"
		   "December")
		 (1- mn))
	    (format nil "~a" dy)
	    (format nil "~a" yr)
	    (format nil "~2,'0d:~2,'0d:~2,'0d" hr min sec))))

(defun pretty-date-ut (&optional (tm (get-universal-time)))
  (multiple-value-bind (sec min hr dy mn yr)
      (decode-universal-time tm)
    (pretty-date yr mn dy hr min sec)))

(defun date-string (&optional (ut (get-universal-time)))
  (if (typep ut 'integer)
      (multiple-value-bind (sec min hr day mon year dow daylight-p zone)
	  (decode-universal-time ut)
	(declare (ignore daylight-p zone))
	(format nil "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~] ~d ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~d ~2,'0d:~2,'0d:~2,'0d"
		dow
		day
		(1- mon)
		year
		hr min sec))))

(defun print-seconds (sec)
  (print-float-units sec "sec"))

(defconstant +posix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun posix-time-to-utime (time)
  (+ time +posix-epoch+))

(defun utime-to-posix-time (time)
  (- time +posix-epoch+))

(defvar *monthnames*
	'((1 . "January")
	  (2 . "February")
	  (3 . "March")
	  (4 . "April")
	  (5 . "May")
	  (6 . "June")
	  (7 . "July")
	  (8 . "August")
	  (9 . "September")
	  (10 . "October")
	  (11 . "November")
	  (12 . "December")))

(defun monthname (stream arg colon-p at-p &optional width (mincol 0) (colinc 1)
		  (minpad 0) (padchar #\Space))
  (declare (ignore colon-p))
  (let ((monthstring (cdr (assoc arg *monthnames*))))
    (if (not monthstring)
	(return-from monthname nil))
    (let ((truncate (if width
			(min width (length monthstring))
			nil)))
      (format stream
	      (if at-p "~V,V,V,V@A" "~V,V,V,VA")
	      mincol colinc minpad padchar
	      (subseq monthstring 0 truncate)))))

(defvar *daynames*
	'((1 . "Monday")
	  (2 . "Tuesday")
	  (3 . "Wednesday")
	  (4 . "Thursday")
	  (5 . "Friday")
	  (6 . "Saturday")
	  (7 . "Sunday")))

(defun dayname (stream arg colon-p at-p &optional width (mincol 0) (colinc 1)
		(minpad 0) (padchar #\Space))
  (declare (ignore colon-p))
  (let ((daystring (cdr (assoc arg *daynames*))))
    (if (not daystring)
	(return-from dayname nil))
    (let ((truncate (if width
			(min width (length daystring))
			nil)))
      (format stream
	      (if at-p "~V,V,V,V@A" "~V,V,V,VA")
	      mincol colinc minpad padchar
	      (subseq daystring 0 truncate)))))

(defconstant* +zellers-adj+ #(0 3 2 5 0 3 5 1 4 6 2 4))

(defun day-of-week (year month day)
  (when (< month 3)
    (decf year))
  (mod
    (+ year (floor year 4) (- (floor year 100)) (floor year 400)
       (aref +zellers-adj+ (1- month)) day)
    7))

;;; datetime.lisp ends here
