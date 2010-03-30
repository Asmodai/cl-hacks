;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; clos.lisp --- Several new CLOS classes and some hacks.
;;; Revision:   17
;;;
;;; Time-stamp: <Tuesday Mar 30, 2010 09:07:08 asmodai>
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2002 Keven M. Rosenberg
;;; Copyright (c) 2002 Tim Bradshaw
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Tue Sep  1 19:00:00 2009
;;; Keywords:   Common Lisp CLOS Hacks
;;; URL:        http://unixware.kicks-ass.org/
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

#-genera
(in-package #:cl-hacks)

;;; ===================================================================
;;; {{{ Singleton classes:                   (Source: Tim Bradshaw)

(defclass singleton-class (cl-hacks-mop:standard-class)
  ((instance :initform nil))
  (:documentation
   "The singleton class metaclass."))

(defmethod cl-hacks-mop:validate-superclass ((class singleton-class)
				(superclass cl-hacks-mop:standard-class))
  ;; It's OK for a standard class to be a superclass of a singlton
  ;; class.
  t)

(defmethod cl-hacks-mop:validate-superclass ((class singleton-class)
				(superclass singleton-class))
  ;; It's OK for a singleton class to be a subclass of a singlton
  ;; class
  t)

(defmethod cl-hacks-mop:validate-superclass ((class cl-hacks-mop:standard-class)
				(superclass singleton-class))
  ;; It's not OK for a standard class which is not a singleton
  ;; class to be a subclass of a singleton class.
  nil)

(defmethod make-instance ((class singleton-class) &key)
  (with-slots (instance) class
    (or instance
	(setf instance (call-next-method)))))

(defvar *singleton-classes* '()
  "A list of current singleton class.")

(defmethod initialize-instance :after ((c cl-hacks::singleton-class) &key)
  (pushnew c *singleton-classes*))

(defun reset-singleton-classes ()
  "Resets all singleton classes so they can be reinstantiated."
  (loop for c in *singleton-classes*
    do (setf (slot-value c 'instance) nil)))

#||
(defclass foo ()
  ()
  (:metaclass singleton-class))
||#

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ Abstract classes:                    (Source: Tim Bradshaw)

;;; Full abstract base classes are classes either explicitly declared
;;; to be abstract, or which contain abstract (unimplemented)
;;; methods. Except the instantiation capability, they have the same
;;; capabilities as a concrete class or type.

(defclass abstract-class (cl-hacks-mop:standard-class)
  ()
  (:documentation "The abstract class metaclass."))

(defmethod make-instance ((c abstract-class) &rest junk)
  (declare (ignore junk))
  (error "Trying to make an instance of ~A which is an abstract class."
	 (class-name c)))

(defmethod cl-hacks-mop:validate-superclass ((class abstract-class)
				(superclass cl-hacks-mop:standard-class))
  ;; This is, in general, somewhat too permissive, but we are going to
  ;; allow any instance of (a subclass of) STANDARD-CLASS to act as a
  ;; superclass of any instance of ABSTRACT-CLASS.
  t)

(defmethod cl-hacks-mop:validate-superclass ((class cl-hacks-mop:standard-class)
				(superclass abstract-class))
  ;; ... and the other way around.
  t)

(defmacro define-abstract-class (class supers slots &rest options)
  "Define an abstract class."
  (when (assoc ':metaclass options)
    (error "Defining an abstract class with a metaclass?"))
  `(defclass ,class ,supers ,slots
     ,@options
     (:metaclass abstract-clas)))

#||
(define-abstract-class some-abstract-thing ()
  ((s :accessor thing-s)))

(defclass thing (abstract-thing)
  ((s :initform 1)))
||#

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ Final classes:                       (Source: Tim Bradshaw)

;;; A final class cannot be subclassed. This is done for reasons of
;;; security and efficiency. 

(defclass final-class (cl-hacks-mop:standard-class)
  ()
  (:documentation "The final class metaclass."))

(defmethod cl-hacks-mop:validate-superclass ((class final-class)
				(superclass cl-hacks-mop:standard-class))
  ;; This is, in general, somewhat too permissive, but we are going to
  ;; allow any instance of (a subclass of) STANDARD-CLASS to act as a
  ;; superclass of any instance of FINAL-CLASS.
  t)

(defmethod cl-hacks-mop:validate-superclass ((class cl-hacks-mop:standard-class)
				(superclass final-class))
  ;; One just can't subclass a final class.
  (error "Attempting to subclass a final class."))

(defmacro define-final-class (class supers slots &rest options)
  "Define a final class."
  (when (assoc ':metaclass options)
    (error "Defining a final class with a metaclass?"))
  `(defclass ,class ,supers ,slots
     @,options
     (:metaclass final-class)))

;;; }}}
;;; ===================================================================

;;; clos.lisp ends here
