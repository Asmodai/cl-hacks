;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; strmatch.lisp --- String matching.
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:33:01 asmodai>
;;; Revision:   3
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

(defun score-multiword-search (s1 s2)
  (let* ((word-list-1 (if (stringp s1)
			  (split-alphanumeric-string s1)
			  s1))
	 (word-list-2 (split-alphanumeric-string s2))
	 (n1 (length word-list-1))
	 (n2 (length word-list-2))
	 (unmatched n1)
	 (score 0))
    (declare (fixnum n1 n2 unmatched score))
    (decf score (* 4 (abs (- n1 n2))))
    (dotimes (iword n1)
      (declare (fixnum iword))
      (let ((w1 (nth iword word-list-1))
	    pos)
	(cond ((consp w1)
	       (let ((first t))
		 (dotimes (i-alt (length w1))
		   (setq pos
			 (position (nth i-alt w1) word-list-2
				   :test #'string-equal))
		   (when pos
		     (incf score (- 30
				    (if first 0 5)
				    (abs (- iword pos))))
		     (decf unmatched)
		     (return))
		   (setq first nil))))
	      ((stringp w1)
	       (awhen (position w1 word-list-2
				:test #'string-equal)
		      (incf score (- 30 (abs (- it iword))))
		      (decf unmatched))))))
    (decf score (* 4 unmatched))
    score))

(defun multiword-match (s1 s2)
  (let* ((word-list-1 (split-alphanumeric-string s1))
	 (word-list-2 (split-alphanumeric-string s2))
	 (n1 (length word-list-1))
	 (n2 (length word-list-2)))
    (when (= n1 n2)
      (dolist (w word-list-1)
	(let ((p (position w word-list-2 :test #'string-equal)))
	  (unless p
	    (return-from multiword-match nil))
	  (setf (nth p word-list-2) "")))
      t)))

;;; strmatch.lisp ends here
