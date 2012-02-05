;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; macros.lisp --- Various macros
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:44:56 asmodai>
;;; Revision:   7
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 17:12:41
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
(in-package #:cl-hacks)

;;; Source: kmrcl
(defmacro with-each-stream-line ((var stream) &body body)
  (let ((eof (gensym))
        (eof-value (gensym))
        (strm (gensym)))
    `(let ((,strm ,stream)
           (,eof ',eof-value))
       (do ((,var (read-line ,strm nil ,eof)
                  (read-line ,strm nil ,eof)))
           ((eql ,var ,eof))
         ,@body))))

;;; Source: kmrcl
(defmacro with-each-file-line ((var filespec) &body body)
  (let ((stream (gensym)))
    `(with-open-file (,stream ,filespec :direction :input)
       (with-each-stream-line (,var ,stream)
         ,@body))))

;;; Source: On Lisp
(defmacro in (needle &rest haystack)
  (let ((insym (gensym)))
    `(let ((,insym ,needle))
       (or ,@(mapcar #'(lambda (c)
                         `(eql ,insym ,c))
                     haystack)))))

;;; Source: On Lisp
(defmacro mean (&rest args)
  `(/ (+ ,@args) ,(length args)))

;;; Source: kmrcl
(defmacro time-seconds (&body body)
  (let ((t1 (gensym)))
    `(let ((,t1 (get-internal-real-time)))
       (values
        (progn ,@body)
        (coerce (/ (- (get-internal-real-time) ,t1)
                   internal-time-units-per-second)
                'double-float)))))

;;; Source: kmrcl
(defmacro time-iterations (n &body body)
  (let ((i (gensym))
        (count (gensym)))
    `(progn
       (let ((,count ,n))
         (format t "~&; Test with ~D iterations: ~W"
                 ,count
                 (quote ,body))
         (let ((t1 (get-internal-real-time)))
           (dotimes ,i ,count)
           ,@body)
         (let* ((t2 (get-internal-real-time))
                (secs (coerce (/ (- t2 t1)
                                 internal-time-units-per-second)
                              'double-float)))
           (format t "~&; Total time: ")
           (print-seconds secs)
           (format t ", time per iteration: ")
           (print-seconds (coerce (/ secs ,n) 'double-float)))))))

;;; Source: kmrcl
(defmacro def-cached-vector (name element-type)
  (let ((get-name (concat-symbol "get-" name "-vector"))
        (release-name (concat-symbol "release-" name "-vector"))
        (table-name (concat-symbol "*cached-" name "-table*"))
        (lock-name (concat-symbol "*cached-" name "-lock*")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,table-name (make-hash-table :test 'equal))
       (defvar ,lock-name (cl-hacks::make-lock ,name))
       (defun ,get-name (size)
         (cl-hacks::with-lock-held (,lock-name)
           (let ((buffers (gethash (cons size ,element-type)
                                   ,table-name) buffers)
                 buffer)
             (make-array size :element-type ,element-type))))
       (defun ,release-name (buffer)
         (cl-hacks::with-lock-held (,lock-name)
           (let ((buffers (gethash (cons size ,element-type)
                                   ,table-name) buffers)
                 buffer)
             (make-array size :element-type ,element-type))))
       (defun ,release-name (buffer)
         (cl-hacks::with-lock-held (,lock-name)
           (let ((buffers (gethash (cons (array-total-size buffer)
                                         ,element-type)
                                   ,table-name)))
             (setf (gethash (cons (array-total-size buffer)
                                  ,element-type) ,table-name)
                   (cons buffer buffers))))))))

;;; Source: kmrcl
(defmacro def-cached-instance (name)
  (let* ((new-name (concat-symbol "new-" name "-instance"))
         (release-name (concat-symbol "release-" name "-instance"))
         (cache-name (concat-symbol "*cached-" name "-instance-table*"))
         (lock-name (concat-symbol "*cached-" name "-instance-lock*")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,cache-name nil)
       (defvar ,lock-name (cl-hacks::make-lock ',name))
       (defun ,new-name ()
         (cl-hacks::with-lock-held (,lock-name)
           (if ,cache-name
               (pop ,cache-name)
               (make-instance ',name))))
       (defun ,release-name (instance)
         (cl-hacks::with-lock-held (,lock-name)
           (push instance ,cache-name))))))

(defmacro pprint-macro (expr)
  `(pprint (macroexpand ',expr)))

(defmacro pprint-macro-1 (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro ppmx (form)
  `(let* ((exp1 (macroexpand-1 ',form))
          (expr (macroexpand ',form))
          (*print-circle* nil))
     (cond ((eql expr exp1)
            (format t "~&; Macro expansion:")
            (pprint expr))
           (t
            (format t "~&; First step of expansion:")
            (pprint exp1)))
     (format t "~%~%")
     (values)))

;;; macros.lisp ends here
