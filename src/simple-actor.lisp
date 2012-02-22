;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; simple-actor.lisp --- Simple actors.
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 06:28:22 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Based on code from BKNR.
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    05 Feb 2012 05:53:40
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

(defclass simple-actor ()
  ((name
    :initarg :name
    :accessor simple-actor-name)
   (process
    :accessor simple-actor-process))
  (:default-initargs
   :name "anonymous actor"))

(defmethod print-object ((actor simple-actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)
    (format stream "\"~A\" (~:[not running~;running~])"
            (simple-actor-name actor)
            (actor-running-p actor))))

(defgeneric run-actor-function (actor)
  (:method ((actor simple-actor))))

(defgeneric stop-actor (actor)
  (:method ((actor simple-actor))
    (when (slot-boundp actor 'process)
      (destroy-process (simple-actor-process actor))
      (slot-makunbound actor 'process))))

(defgeneric start-actor (actor)
  (:method ((actor simple-actor))
    (stop-actor actor)
    (setf (slot-value actor 'process)
          (make-process (lambda ()
                          (funcall #'run-actor-function actor))
                        :name (simple-actor-name actor)))))

(defgeneric actor-running-p (actor)
  (:method ((actor simple-actor))
    (and (slot-boundp actor 'process)
         (process-active-p (simple-actor-process actor)))))

;;; simple-actor.lisp ends here
