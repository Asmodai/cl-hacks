;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; functions.lisp --- Functions
;;;
;;; Time-stamp: <Monday Dec  5, 2011 05:05:30 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Dec 2011 02:57:51
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
;;; {{{ Commentary:
;;;
;;; }}}

#-genera
(in-package #:cl-hacks-internals)

(declaim (inline ensure-function))
(declaim (ftype (function (t) (values function &optional))
                ensure-function))
(defun ensure-function (function-designator)
  (if (functionp function-designator)
      function-designator
      (fdefinition function-designator)))

(defun disjoin (predicate &rest more-predicates)
(declare (optimize (speed 3)
                   (safety 1)
                   (debug 1)))
(let ((predicate (ensure-function predicate))
      (more-predicates (mapcar #'ensure-function more-predicates)))
  (lambda (&rest arguments)
    (or (apply predicate arguments)
        (some (lambda (p)
                (declare (type function p))
                (apply p arguments))
              more-predicates)))))

(defun conjoin (predicate &rest more-predicates)
  (lambda (&rest arguments)
    (and (apply predicate arguments)
         ;; Cannot simply use CL:EVERY because we want to return the
         ;; non-NIL value of the last predicate if all succeed.
         (do ((tail (cdr more-predicates) (cdr tail))
              (head (car more-predicates) (car tail)))
             ((not tail)
              (apply head arguments))
           (unless (apply head arguments)
             (return nil))))))

(defun compose (function &rest more-functions)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 1)))
  (reduce (lambda (f g)
            (let ((f (ensure-function f))
                  (g (ensure-function g)))
              (lambda (&rest arguments)
                (declare (dynamic-extent arguments))
                (funcall f (apply g arguments)))))
          more-functions
          :initial-value function))

(define-compiler-macro compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "COMPOSE")))
      `(let ,(loop for f in funs for arg in args
                   collect `(,f (ensure-function ,arg)))
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

(defun multiple-value-compose (function &rest more-functions)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 1)))
  (reduce (lambda (f g)
            (let ((f (ensure-function f))
                  (g (ensure-function g)))
              (lambda (&rest arguments)
                (declare (dynamic-extent arguments))
                (multiple-value-call f (apply g arguments)))))
          more-functions
          :initial-value function))

(define-compiler-macro multiple-value-compose (function
                                               &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(multiple-value-call
                      ,(car funs)
                    ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "MV-COMPOSE")))
      `(let ,(mapcar #'list funs args)
         (declare (optimize (speed 3)
                            (safety 1)
                            (debug 1)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

(defun curry (function &rest arguments)
(declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      ;; Using M-V-C we don't need to append the arguments.
      (multiple-value-call fn
        (values-list arguments)
        (values-list more)))))

(define-compiler-macro curry (function &rest arguments)
  (let ((curries (make-gensym-list (length arguments) "CURRY")))
    `(let ,(mapcar #'list curries arguments)
       (declare (optimize (speed 3)
                          (safety 1)
                          (debug 1)))
       (lambda (&rest more)
         (apply ,function ,@curries more)))))

(defun rcurry (function &rest arguments)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 1)))
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      (multiple-value-call fn
        (values-list more)
        (values-list arguments)))))

;;; functions.lisp ends here
