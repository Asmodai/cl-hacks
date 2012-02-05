;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-MOP; Base: 10; Lowercase: Yes -*-
;;;
;;; mop-extensions.lisp --- MOP extensions
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:40:42 asmodai>
;;; Revision:   15
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 17:23:06
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
(in-package #:cl-hacks-mop)

#+sbcl
(if (find-package 'sb-mop)
    (pushnew :cl-hacks-sbcl-mop cl:*features*)
    (pushnew :cl-hacks-sbcl-pcl cl:*features*))

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (eq (symbol-package 'pcl:find-class)
          (find-package 'common-lisp))
      (pushnew :cl-hacks-cmu-mop cl:*features*)
      (pushnew :cl-hacks-cmu-pcl cl:*features*)))

#+lispworks
(defun intern-eql-specializer (slot)
  `(eql ,slot))

(defmacro process-class-option (metaclass slot-name &optional required)
  #+lispworks
  `(defmethod clos:process-a-class-option
       ((class ,metaclass)
        (name (eql ,slot-name))
        value)
     (when (and ,required (null value))
       (error "Metaclass ~A class slot ~A must have a value."
              (quote ,metaclass)
              name))
     (list name `',value))
  #-lispworks
  (declare (ignore metaclass slot-name required)))

(defmacro process-slot-option (metaclass slot-name)
  #+lispworks
  `(defmethod clos:process-a-slot-option
       ((class ,metaclass)
        (option (eql ,slot-name))
        value
        already-processed-options
        slot)
     (list* option `',value already-processed-options))
  #-lispworks
  (declare (ignore metaclass slot-name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(or sbcl genera)
  (error "You need to edit MOP-EXTENSIONS.LISP and add the correct ~
         ~imports for your Lisp environment.")

  (import
   ;;
   ;; SBCL
   #+sbcl
   '(#+cl-hacks-sbcl-mop sb-mop::class-of
     #-cl-hacks-sbcl-mop sb-pcl::class-of
     #+cl-hacks-sbcl-mop sb-mop::class-name
     #-cl-hacks-sbcl-mop sb-pcl::class-name
     #+cl-hacks-sbcl-mop sb-mop::class-slots
     #-cl-hacks-sbcl-mop sb-pcl::class-slots
     #+cl-hacks-sbcl-mop sb-mop::find-class
     #-cl-hacks-sbcl-mop sb-pcl::find-class
     sb-pcl::standard-class
     sb-pcl::compute-class-precedence-list
     sb-pcl::compute-effective-slot-definition
     sb-pcl::compute-effective-slot-definition-initargs
     sb-pcl::direct-slot-definition-class
     sb-pcl::effective-slot-definition-class
     sb-pcl::slot-value-using-class
     sb-pcl::slot-definition-name
     sb-pcl::slot-definition-initform
     sb-pcl::slot-definition-type
     sb-pcl::slot-definition-writers
     sb-pcl::slot-definition-readers
     sb-pcl::generic-function-name
     sb-pcl::generic-function-lambda-list
     sb-pcl::generic-function-method-class
     sb-pcl::standard-direct-slot-definition
     sb-pcl::standard-effective-slot-definition     
     sb-pcl::finalize-inheritance
     sb-pcl::validate-superclass
     sb-pcl::class-direct-slots
     sb-pcl::class-prototype
     sb-pcl::intern-eql-specializer
     sb-pcl::make-method-lambda
     sb-pcl::compute-slots)
   ;;
   ;; Symbolics Genera (Future Common Lisp)
   #+genera
   '(class-of
     class-name
     class-slots
     find-class
     clos::compute-class-precedence-list
     clos::compute-effective-slot-definition
     clos::compute-effective-slot-definition-initargs
     clos::direct-slot-definition-class
     clos::effective-slot-definition-class
     clos::standard-class
     clos::slot-value-using-class
     clos::slot-definition-name
     clos::slot-definition-initform
     clos::slot-definition-type
     clos::slot-definition-writers
     clos::slot-definition-readers
     clos::generic-function-name
     clos::generic-function-lambda-list
     clos::generic-function-method-class
     clos::standard-direct-slot-definition
     clos::standard-effective-slot-definition     
     clos::finalize-inheritance
     clos::validate-superclass
     clos::class-direct-slots
     clos::class-prototype
     clos::intern-eql-specializer
     clos::make-method-lambda
     clos::compute-slots)))

;;;
;;; Now export our symbols
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (fboundp 'class-slots)
    (export '(class-of
              class-name
              class-slots
              find-class
              standard-class
              compute-class-precedence-list
              compute-effective-slot-definition
              compute-effective-slot-definition-initargs
              direct-slot-definition-class
              effective-slot-definition-class
              slot-value-using-class
              slot-definition-name
              slot-definition-initform
              slot-definition-type
              slot-definition-writers
              slot-definition-readers
              generic-function-name
              generic-function-lambda-list
              generic-function-method-class
              standard-direct-slot-definition
              standard-effective-slot-definition
              finalize-inheritance
              validate-superclass
              class-direct-slots
              class-prototype
              intern-eql-specializer
              make-method-lambda
              compute-slots
              process-slot-option
              process-class-option)))
    
  (when (>= (length (generic-function-lambda-list
                     (ensure-generic-function
                      'compute-effective-slot-definition)))
            3)
    (pushnew :cl-hacks-normal-cesd cl:*features*))
  
  (when (>= (length (generic-function-lambda-list
                     (ensure-generic-function
                      'direct-slot-definition-class)))
            3)
    (pushnew :cl-hacks-normal-dsdc cl:*features*)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (not (member :cl-hacks-mop cl:*features*))
      (pushnew :cl-hacks-mop cl:*features*)))

;;; mop-extensions.lisp ends here
