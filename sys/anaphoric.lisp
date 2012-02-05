;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; anaphoric.lisp --- Anaphoric functions
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:47:41 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 16:02:53
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

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it
         ,then-form
         ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn
          ,@body)))

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
         `(aif ,(car args)
               (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro alambda (lambda-list &body body)
  `(labels ((self ,lambda-list ,@body))
     #'self))

(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
                        (case (length args)
                          (0 nil)
                          (1 (car args))
                          (t `(let ((it ,(car args)))
                                ,(self (cdr args))))))
               args)))

(defmacro aif2 (test-form &optional then-form else-form)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win)
         ,test-form
       (if (or it ,win)
           ,then-form
           ,else-form))))

(defmacro awhen2 (test-form &body body)
  `(aif2 ,test-form
         (progn ,@body)))

(defmacro awhile2 (test-form &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
         (aif2 ,test-form
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

;;; anaphoric.lisp ends here
