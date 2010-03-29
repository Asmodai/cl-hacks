;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; clos.lisp --- Several new CLOS classes and some hacks.
;;; Revision:   12
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:25:08 asmodai>
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

(defclass singleton-class (standard-class)
  ((instance :initform nil))
  (:documentation
   "The singleton class metaclass."))

(defmethod validate-superclass ((class singleton-class)
				(superclass standard-class))
  ;; It's OK for a standard class to be a superclass of a singlton
  ;; class.
  t)

(defmethod validate-superclass ((class singleton-class)
				(superclass singleton-class))
  ;; It's OK for a singleton class to be a subclass of a singlton
  ;; class
  t)

(defmethod validate-superclass ((class standard-class)
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

(defclass abstract-class (standard-class)
  ()
  (:documentation "The abstract class metaclass."))

(defmethod make-instance ((c abstract-class) &rest junk)
  (declare (ignore junk))
  (error "Trying to make an instance of ~A which is an abstract class."
	 (class-name c)))

(defmethod validate-superclass ((class abstract-class)
				(superclass standard-class))
  ;; This is, in general, somewhat too permissive, but we are going to
  ;; allow any instance of (a subclass of) STANDARD-CLASS to act as a
  ;; superclass of any instance of ABSTRACT-CLASS.
  t)

(defmethod validate-superclass ((class standard-class)
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

(defclass final-class (standard-class)
  ()
  (:documentation "The final class metaclass."))

(defmethod validate-superclass ((class final-class)
				(superclass standard-class))
  ;; This is, in general, somewhat too permissive, but we are going to
  ;; allow any instance of (a subclass of) STANDARD-CLASS to act as a
  ;; superclass of any instance of FINAL-CLASS.
  t)

(defmethod validate-superclass ((class standard-class)
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

;;; ===================================================================
;;; {{{ Attribute classes:                          (Source: KMRCL)

(defclass attributes-class (standard-class)
  ()
  (:documentation "A metaclass that implements attributes on slots."))

(defclass attributes-dsd (standard-direct-slot-definition)
  ((attributes :initarg :attributes
	       :initform nil
	       :accessor dsd-attributes)))

(defclass attributes-esd (standard-effectives-slot-definition)
  ((attributes :initarg :attributes
	       :initform nil
	       :accessor esd-attributes)))

#-genera
(process-slot-option attributes-class :attributes)

#+(or cmu scl sbcl openmcl genera)
(defmethod validate-superclass ((class attributes-class)
				(superclass standard-class))
  t)

(defmethod direct-slot-definition-class ((cl attributes-class)
					 #+cl-hacks-normal-dsdc &rest initargs)
  (declare (ignore initargs))
  (find-class 'attributes-dsd))

(defmethod effective-slot-definition-class ((cl attributes-class)
					    #+cl-hacks-normal-dsdc &rest initargs)
  (declare (ignore initargs))
  (find-class 'attributes-esd))

(defmethod compute-effective-slot-definition ((cl attributes-class)
					      #+cl-hacks-mop-normal-cesd name dsds)
  #+cl-hacks-normal-cesd (declare (ignore name))
  (let ((esd (call-next-method)))
    (setf (esd-attributes esd) (remove-duplicates (cl-hacks:mapappend #'dsd-attributes dsds)))
    esd))

(defmethod compute-slots ((class attributes-class))
  (let* ((normal-slots (call-next-method))
	 (alist (mapcar #'(lambda (slot)
			    (cons (slot-definition-name slot)
				  (mapcar #'(lambda (attr) (list attr))
					  (esd-attributes slot))))
			normal-slots)))
    (cons (make-instance 'attributes-esd
			 :name 'all-attributes
			 :initform `',alist
			 :initfunction #'(lambda () alist)
			 :allocation :instance
			 :documentation "Attribute bucket"
			 :type t)
	  normal-slots)))

(defun slot-attributes (instance slot-name attribute)
  (cdr (slot-attribute-bucket instance slot-name attribute)))

(defun (setf slot-attribute) (new-value instance slot-name attribute)
  (setf (cdr (slot-attribute-bucket instance slot-name attribute))
	new-value))

(defun slot-attribute-bucket (instance slot-name attribute)
  (let* ((all-buckets (slot-value instance 'all-attributes))
	 (slot-bucket (assoc slot-name all-buckets)))
    (unless slot-bucket
      (error "The slot named ~s of ~s has no attributes."
	     slot-name instance))
    (let ((attr-bucket (assoc attribute (cdr slot-bucket))))
      (unless attr-bucket
	(error "The slot named ~s of ~s has no attributes named ~s."
	       slot-name instance attribute))
      attr-bucket)))
		  
;;; }}}
;;; ===================================================================

;;; clos.lisp ends here
