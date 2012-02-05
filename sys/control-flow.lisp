;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; control-flow.lisp --- Control flow functions
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:48:39 asmodai>
;;; Revision:   6
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    27 Nov 2011 02:21:23
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

;;; ==================================================================
;;;{{{ Utility functions:

(defun extract-function-name (spec)
  (if (and (consp spec)
           (member (first spec) '(quote function)))
      (second spec)
      spec))

(defun generate-switch-body (whole object clauses test key
                             &optional default)
  (with-gensyms (value)
    (setf test (extract-function-name test))
    (setf key (extract-function-name key))
    (when (and (consp default)
               (member (first default) '(error cerror)))
      (setf default 
            `(,@default
                 "No keys match in SWITCH. Testing against ~S with ~S."
                 ,value ',test)))
    `(let ((,value (,key ,object)))
      (cond ,@(mapcar (lambda (clause)
                        (if (member (first clause) '(t otherwise))
                            (progn
                              (when default
                                (error
                                 "Multiple default clauses or illegal ~
                                  use of a default clause in ~S."
                                 whole))
                              (setf default `(progn ,@(rest clause)))
                              '(()))
                            (destructuring-bind (key-form &body forms)
                                clause
                              `((,test ,value ,key-form)
                                ,@forms))))
                      clauses)
            (t ,default)))))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Switch macros:

(defmacro switch (&whole whole
                  (object &key (test 'eql) (key 'identity))
                  &body clauses)
  (generate-switch-body whole object clauses test key))

(defmacro eswitch (&whole whole
                   (object &key (test 'eql) (key 'identity))
                   &body clauses)
  (generate-switch-body whole object clauses test key '(error)))

(defmacro cswitch (&whole whole
                   (object &key (test 'eql) (key 'identity))
                   &body clauses)
  (generate-switch-body whole object clauses test key
                        '(cerror "Return NIL from CSWITCH.")))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Conditional evaluation:

(defmacro whichever (&rest possibilities &environment env)
  (setf possibilities (mapcar (lambda (p)
                                (macroexpand p env)) possibilities))
  (if (every (lambda (p)
               (constantp p)) possibilities)
      `(svref (load-time-value (vector ,@possibilities))
              (random ,(length possibilities)))
      (with-gensyms (funct)
        `(let ((,funct (lambda ()
                            ,(pop possibilities))))
           (declare (function ,function))
           ,@(let ((p 1))
               (mapcar (lambda (possibility)
                         `(when (zerop (random ,(incf p)))
                            (setf ,funct (lambda ()
   ,possibility))))
                       possibilities))
           (funcall ,funct)))))

(defmacro xor (&rest datums)
  (with-gensyms (xor tmp true)
    `(let (,tmp ,true)
       (block ,xor
         ,@(mapcar (lambda (datum)
                     `(if (setf ,tmp ,datum)
                          (if ,true
                              (return-from ,xor (values nil nil))
                              (setf ,true ,tmp))))
                   datums)
         (return-from ,xor (values ,true t))))))

(defmacro nth-value-or (value &body forms)
  (once-only (value)
    (with-gensyms (values)
      `(let ((,values (multiple-value-list ,(first forms))))
         (if (nth ,value ,values)
             (values-list ,values)
             ,(if (rest forms)
                  `(nth-value-or ,value ,@(rest forms))
                  nil))))))

;;;}}}
;;; ==================================================================

;;; control-flow.lisp ends here
