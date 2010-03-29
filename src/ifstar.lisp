;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; ifstar.lisp --- Franz Incs' IF* implementation
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:29:08 asmodai>
;;; Revision:   2
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar if*-keyword-list '("then" "thenret" "else" "elseif")))

(defmacro if* (&rest args)
  (do ((xx (reverse args) (cdr xx))
       (state :init)
       (elseseen nil)
       (totalcol nil)
       (lookat nil)
       (col nil))
      ((null xx)
       (cond ((eq state :compl)
	      `(cond ,@totalcol))
	     (t
	      (error "if*: illegal form ~s" args))))
    (cond ((and (symbolp (car xx))
		(member (symbol-name (car xx))
			if*-keyword-list
			:test #'string-equal))
	   (setq lookat (symbol-name (car xx)))))
    (cond ((eq state :init)
	   (cond (lookat (cond ((string-equal lookat "thenret")
				(setq col nil
				      state :then))
			       (t
				(error "if*: bad keyword ~s" lookat))))
		 (t
		  (setq state :col
			col nil)
		  (push (car xx) col))))
	  ((eq state :col)
	   (cond (lookat
		  (cond ((string-equal lookat "else")
			 (cond (elseseen
				(error "if*: multiple elses")))
			 (setq elseseen t
			       state :init)
			 (push `(t ,@col) totalcol))
			((string-equal lookat "then")
			 (setq state :then))
			(t
			 (error "if*: bad keyword ~s" lookat))))
		 (t
		  (push (car xx) col))))
	  ((eq state :then)
	   (cond (lookat
		  (error "if*: keyword ~s at the wrong place" (car xx)))
		 (t
		  (setq state :compl)
		  (push `(,(car xx) ,@col) totalcol))))
	  ((eq state :compl)
	   (cond ((not (string-equal lookat "elseif"))
		  (error "if*: missing elseif clause")))
	   (setq state :init)))))

;;; ifstar.lisp ends here
