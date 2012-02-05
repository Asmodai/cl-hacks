;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-CLOS; Base: 10; Lowercase: Yes -*-
;;;
;;; classes.lisp --- CLOS extensions
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:39:32 asmodai>
;;; Revision:   5
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 16:26:03
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
(in-package #:cl-hacks-clos)

;;; ==================================================================
;;;{{{ Singleton classes:                   (Source: Tim Bradshaw)

(defclass singleton-class (cl-hacks-mop:standard-class)
  ((instance
    :initform nil)))

(defmethod cl-hacks-mop:validate-superclass
    ((class singleton-class)
     (superclass cl-hacks-mop:standard-class))
  t)

(defmethod cl-hacks-mop:validate-superclass
    ((class singleton-class)
     (superclass singleton-class))
  t)

(defmethod cl-hacks-mop:validate-superclass
    ((class cl-hacks-mop:standard-class)
     (superclass singleton-class))
  nil)

(defmethod make-instance ((class singleton-class) &key)
  (with-slots (instance)
              class
     (or instance
         (setf instance (call-next-method)))))

(defvar *singleton-classes* '()
  "A list of current singleton classes.")

(defmethod initialize-instance :after ((c singleton-class) &key)
  (pushnew c *singleton-classes*))

(defun reset-singleton-classes ()
  "Resets all singleton classes so they can be reinstated."
  (loop for c in *singleton-classes*
        do (setf (slot-value c 'instance) nil)))

#||
(defclass foo ()
  ()
  (:metaclass singleton-class))
||#

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Abstract classes:                    (Source: Tim Bradshaw)

(defclass abstract-class (cl-hacks-mop:standard-class)
  ())

(defmethod make-instance ((c abstract-class) &rest junk)
  (declare (ignore junk))
  (error "Trying to instantiate the abstract class ~A."
         (class-name c)))

(defmethod cl-hacks-mop:validate-superclass
    ((class abstract-class)
     (superclass cl-hacks-mop:standard-class))
  t)

(defmethod cl-hacks-mop:validate-superclass
    ((class cl-hacks-mop:standard-class)
     (superclass abstract-class))
  t)

(defmacro define-abstract-class (class supers slots &rest options)
  "Define an abstract class.  See DEFCLASS for usage."
  (when (assoc ':metaclass options)
    (Error "Cannot define an abstract class with a metaclass."))
  `(defclass ,class ,supers
     ,slots
     ,@options
     (:metaclass abstract-class)))

#||

(define-abstract-class abstract-thing ()
  ((s :accessor thing-s)))

(defclass thing (abstract-thing)
  ((s :initform 1)))

||#

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Final classes:                       (Source: Tim Bradshaw)

(defclass final-class (cl-hacks-mop:standard-class)
  ())

(defmethod cl-hacks-mop:validate-superclass
    ((class final-class)
     (superclass cl-hacks-mop:standard-class))
  t)

(defmethod cl-hacks-mop:validate-superclass
    ((class cl-hacks-mop:standard-class)
     (superclass final-class))
  (error "You cannot subclass the final class ~A."
         (class-name superclass)))

(defmacro define-final-class (class supers slots &rest options)
  "Define a final class.  See DEFCLASS for usage."
  (when (assoc ':metaclass options)
    (error "Cannot define a final class with a metaclass."))
  `(defclass ,class ,supers
     ,slots
     @,options
     (:metaclass final-class)))

;;;}}}
;;; ==================================================================

;;; classes.lisp ends here
