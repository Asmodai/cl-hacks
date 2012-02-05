;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; symbols.lisp --- Symbol functions
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:53:46 asmodai>
;;; Revision:   7
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Dec 2011 01:04:48
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
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
;;;}}}
;;;{{{ Commentary:
;;;
;;;}}}

#-genera
(in-package #:cl-hacks-internals)

(defsubst ensure-symbol (name &optional (package *package*))
  (intern (string name) package))

(defun maybe-intern (name package)
  (values
   (if package
       (intern name (if (eq t package) *package* package))
       (make-symbol name))))

(defsubst format-symbol (package control &rest arguments)
  (maybe-intern (apply #'format nil control arguments) package))

(defun make-keyword (name)
  (intern (string name) :keyword))

(defun make-gensym (name)
  (gensym (if (typep name '(integer 0))
              name
              (string name))))

(defun make-gensym-list (length &optional (x "G"))
  (let ((g (if (typep x '(integer 0)) x (string x))))
    (loop repeat length
          collect (gensym g))))

(defun symbolicate (&rest things)
  (let* ((length (reduce #'+ things
                         :key (lambda (x) (length (string x)))))
         (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (thing things (values (intern name)))
        (let* ((x (string thing))
               (len (length x)))
          (replace name x :start1 index)
          (incf index len))))))

;;; symbols.lisp ends here
