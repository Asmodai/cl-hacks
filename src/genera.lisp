;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; genera.lisp --- Genera-specific hacks
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:28:07 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Sep 2009 21:00:17
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
(error "Why do you want these functions in a Common Lisp that probably
already has them?")


(defun read-sequence (seq stream &key (start 0) end) 
  (let* ((seq-end (if end end (length seq)))
         (res (scl:send stream ':string-in nil seq start seq-end)))
    res))

(defun write-sequence (seq stream &key (start 0) end)
  (let ((seq-end (if end end (length seq))))
    (scl:send stream ':string-out seq start seq-end)))

;; This is ugly and needs to be re-written
(defun ensure-directories-exist (pathspec &key (verbose nil))
  (values pathspec
	  (if (probe-directory pathspec)
	      nil
	      (progn
		(cp:execute-command "Create Directory" pathspec)
		t))))

;; Gnarly
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (fboundp 'read-sequence)
	     (fboundp 'write-sequence)
	     (fboundp 'ensure-directories-exist))
    (let ((read-symb (find-symbol "READ-SEQUENCE"))
	  (write-symb (find-symbol "WRITE-SEQUENCE"))
	  (ensure-dir (find-symbol "ENSURE-DIRECTORIES-EXIST")))
      (import read-symb (find-package 'future-common-lisp))
      (import write-symb (find-package 'future-common-lisp))
      (import ensure-dir (find-package 'future-common-lisp)))))

;; EOF
