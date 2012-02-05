;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; hash-tables.lisp --- Hash tables
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:51:25 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Dec 2011 14:40:55
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

(defun copy-hash-table (table &key key test size
                        rehash-size rehash-threshold)
  (setf key (or key 'identity)
        test (or test (hash-table-test table))
        size (or size (hash-table-size table))
        rehash-size (or rehash-size (hash-table-size table))
        rehash-threshold (or rehash-threshold
                             (hash-table-rehash-threshold table)))
  (let ((copy (make-hash-table :test test :size size
                               :rehash-size rehash-size
                               :rehash-threshold rehash-threshold)))
    (maphash (lambda (k v)
               (setf (gethash k copy) (funcall key v)))
             table)
    copy))

(defsubst maphash-keys (function table)
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall function k))
           table))

(defsubst maphash-values (function table)
  (maphash (lambda (k v)
             (declare (ignore k))
             (funcall function v))
           table))

(defun hash-table-keys (table)
  (let ((keys nil))
    (maphash-keys (lambda (k)
                    (push k keys))
                  table)
    keys))

(defun hash-table-values (table)
  (let ((values nil))
    (maphash-values (lambda (v)
                      (push v values))
                    table)
    values))

(defun hash-table-alist (table)
  (let ((alist nil))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    alist))

(defun hash-table-plist (table)
  (let ((plist nil))
    (maphash (lambda (k v)
               (setf plist (list* k v plist)))
             table)
    plist))

(defun alist-hash-table (alist &rest hash-table-initargs)
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (dolist (cons alist)
      (setf (gethash (car cons) table) (cdr cons)))
    table))

(defun plist-hash-table (plist &rest hash-table-initargs)
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (do ((tail plist (cddr tail)))
        ((not tail))
      (setf (gethash (car tail) table) (cadr tail)))
    table))

(defun ensure-gethash (key hash-table &optional default)
  (multiple-value-bind (value ok) (gethash key hash-table)
    (if ok
        (values value ok)
        (values (setf (gethash key hash-table) default) nil))))

;;; hash-tables.lisp ends here
