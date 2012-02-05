;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; symbols.lisp --- Symbol functions.
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:46:57 asmodai>
;;; Revision:   8
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

#-genera
(in-package #:cl-hacks)

(defun cl-symbols ()
  (append (cl-variables) (cl-functions)))

(defun cl-variables ()
  (let ((vars '()))
    (do-symbols (s 'common-lisp)
      (multiple-value-bind (sym status)
          (find-symbol (symbol-name s) 'common-lisp)
        (when (and (or (eq status :external)
                       (eq status :internal))
                   (boundp sym))
          (push sym vars))))
    (nreverse vars)))

(defun cl-functions ()
  (let ((funcs '()))
    (do-symbols (s 'common-lisp)
      (multiple-value-bind (sym status)
          (find-symbol (symbol-name s) 'common-lisp)
        (when (and (or (eq status :external)
                       (eq status :internal))
                   (fboundp sym))
          (push sym funcs))))
    (nreverse funcs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (char= #\a (schar (symbol-name '#:a) 0))
    (pushnew :cl-hacks-lowercase-reader cl:*features*))
  (when (not (string= (symbol-name '#:a)
                      (symbol-name '#:A)))
    (pushnew :cl-hacks-case-sensitive cl:*features*)))

(defun string-default-case (str)
  #+(and (not cl-hacks-lowercase-reader)) (string-upcase str)
  #+(and cl-hacks-lowercase-reader) (string-downcase str))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq cl:*features* (delete :cl-hacks-lowercase-reader cl:*features*)
        cl:*features* (delete :cl-hacks-case-sensitive cl:*features*)))

(defun concat-symbol-pkg (pkg &rest args)
  (declare (dynamic-extent args))
  (flet ((stringify (arg)
           (etypecase arg
             (string 
              (string-upcase arg))
             (symbol
              (symbol-name arg)))))
    (let ((str (apply #'concatenate 'string (mapcar #'stringify args))))
      (nth-value 0 (intern (string-default-case str)
                           (if pkg pkg *package*))))))

(defun concat-symbol (&rest args)
  (apply #'concat-symbol-pkg nil args))

(defun ensure-keyword (name)
  (etypecase name
    (keyword name)
    (string (nth-value 0 (intern (string-default-case name) :keyword)))
    (symbol (nth-value 0 (intern (symbol-name name) :keyword)))))

(defun ensure-keyword-upcase (name)
  (nth-value 0 (intern
                (string-upcase
                 (symbol-name (ensure-keyword name))) :keyword)))

(defun ensure-keyword-default-case (name)
  (nth-value 0 (intern (string-default-case
                        (symbol-name (ensure-keyword name))) :keyword)))

(defun show (&optional (what :variables) (package *package*))
  (ecase what
    (:variables (show-variables package))
    (:functions (show-functions package))))

(defun show-variables (package &optional (stream *standard-output*))
  (format stream "~&; Showing variables for package ~A:~%"
          (package-name package))
  (do-symbols (s package)
    (multiple-value-bind (sym status)
        (find-symbol (symbol-name s) package)
      (when (and (or (eq status :internal)
                     (eq status :external))
                 (boundp sym))
        (format stream "~&; Symbol ~S~T -> ~S~T[~A]~%"
                sym
                (symbol-value sym)
                (string-capitalize (string status)))))))

(defun show-functions (package &optional (stream *standard-output*))
  (format stream "~&; Showing functions for package ~A:~%"
          (package-name package))
  (do-symbols (s package)
    (multiple-value-bind (sym status)
        (find-symbol (symbol-name s) package)
      (when (and (or (eq status :internal)
                     (eq status :external))
                 (fboundp sym))
        (format stream "~&; Function ~S~T -> ~S~T[~A]~%"
                sym
                (symbol-function sym)
                (string-capitalize (string status)))))))

;; symbols.lisp ends here
