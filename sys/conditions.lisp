;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; conditions.lisp --- Conditions
;;;
;;; Time-stamp: <Monday Dec  5, 2011 05:04:52 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    25 Nov 2011 12:36:24
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

;;; ==================================================================
;;; {{{ User code conditions:

(define-condition code (error)
  ((proc
    :initarg :proc
    :initform nil
    :reader code-proc)
   (mesg
    :type (or null simple-string)
    :initarg :mesg
    :initform nil
    :reader code-mesg)
   (args
    :type list
    :initarg :args
    :initform nil
    :reader code-args))
  (:documentation "An error in the user's code.")
  (:report (lambda (cc out)
             (declare (stream out))
             (format out "[~S]~@[ ~?~]"
                     (code-proc cc)
                     (code-mesg cc)
                     (code-args cc)))))

(define-condition case-error (code)
  ((mesg
    :type simple-string
    :initform "`~S' evaluated to `~S', not one of [~@{`~s'~^ ~}]"
    :reader code-mesg))
  (:documentation "An error in a case statement."))

(define-condition not-implemented (code)
  ((mesg
    :type simple-string
    :initform "is not implemented for ~A [~A]"
    :reader code-mesg)
   (args
    :type list
    :initform (list (lisp-implementation-type)
                    (lisp-implementation-version))
    :reader code-args))
  (:documentation "Your implementation does not support this
  functionality."))

(define-condition required-argument-missing (code)
  ((mesg
    :type simple-string
    :initform "required argument(s) ~@{`~S'~^, ~} missing."
    :reader code-mesg)
   (args
    :type list
    :initform '()
    :reader code-args))
  (:documentation "A required argument is missing."))

(defmacro required-argument (fun arg)
  `(error 'required-argument-missing
          :proc ,(if fun
                     fun
                     'required-argument)
          :args (list ,arg)))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Simple conditions:

(define-condition simple-style-warning (style-warning
                                simple-warning)
  ())

(defun simple-style-warning (message &rest args)
  (warn 'simple-style-warning
        :format-control message
        :format-arguments args))

(define-condition simple-reader-error
     #-sbcl (reader-error simple-error)
  #+sbcl (sb-int:simple-reader-error)
  ())

(defun simple-reader-error (stream message &rest args)
  (error 'simple-reader-error
         :stream stream
         :format-control message
         :format-arguments args))

(define-condition simple-parse-error (simple-error
                                      parser-error)
  ())

(defun simple-parse-error (message &rest args)
  (error 'simple-parse-error
         :format-control message
         :format-arguments args))

(define-condition simple-program-error (simple-error
                                        program-error)
  ())

(defun simple-program-error (message &rest args)
  (error 'simple-program-error
         :format-control message
         :format-arguments args))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Macros:

(defmacro ignore-some-conditions ((&rest conditions) &body body)
  `(handler-case
       (progn ,@body)
     ,@(loop for condition in conditions
             collect `(,condition (c) (values nil c)))))

(defmacro unwind-protect-case ((&optional abort-flag)
                               protected-form
                               &body clauses)
  (check-type abort-flag (or null symbol))
  (let ((gflag (gensym "FLAG+")))
    `(let ((,gflag t))
       (unwind-protect
           (multiple-value-prog1 ,protected-form
             (setf ,gflag nil))
         (let ,(and abort-flag `((,abort-flag ,gflag)))
           ,@(loop for (cleanup-kind . forms) in clauses
                   collect (ecase cleanup-kind
                             (:normal
                              `(when (not ,gflag) ,@forms))
                             (:abort
                              `(when ,gflag ,@forms))
                             (:always
                              `(progn ,@forms)))))))))
;;; }}}
;;; ==================================================================

;;; conditions.lisp ends here
