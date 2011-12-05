;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-MOP; Base: 10; Lowercase: Yes -*-
;;;
;;; mop-extensions.lisp --- MOP extensions
;;;
;;; Time-stamp: <Monday Dec  5, 2011 05:44:50 asmodai>
;;; Revision:   5
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 17:23:06
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
(in-package #:cl-hacks-mop)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (find-package 'sb-mop)
      (pushnew :cl-hacks-sbcl-mop cl:*features*)
      (pushnew :cl-hacks-sbcl-pcl cl:*features*)))

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
  (shadowing-import
   ;;
   ;; Allegro Common Lisp
   #+allegro
   '(excl::compute-effective-slot-definition-initargs)
   ;;
   ;; LispWorks
   #+lispworks
   '(clos::compute-effective-slot-definition-initargs)
   ;;
   ;; CLISP
   #+clisp
   '(clos::compute-effective-slot-definition-initargs)
   ;;
   ;; SBCL
   #+sbcl
   '(#+cl-hacks-sbcl-mop class-of
     #-cl-hacks-sbcl-mop sb-pcl:class-of
     #+cl-hacks-sbcl-mop class-name
     #-cl-hacks-sbcl-mop sb-pcl:class-name
     #+cl-hacks-sbcl-mop class-slots
     #-cl-hacks-sbcl-mop sb-pcl:class-slots
     #+cl-hacks-sbcl-mop find-class
     #-cl-hacks-sbcl-mop sb-pcl:find-class
     sb-pcl::standard-class
     sb-pcl:slot-definition-name
     sb-pcl::finalize-inheritance
     sb-pcl::standard-direct-slot-definition
     sb-pcl::standard-effective-slot-definition
     sb-pcl::validate-superclass
     sb-pcl::direct-slot-definition-class
     sb-pcl::effective-slot-definition-class
     sb-pcl::compute-effective-slot-definition
     sb-pcl:class-direct-slots
     sb-pcl::compute-effective-slot-definition-initargs
     sb-pcl::slot-value-using-class
     sb-pcl:class-prototype
     sb-pcl:generic-function-method-class
     sb-pcl:intern-eql-specializer
     sb-pcl:make-method-lambda
     sb-pcl:generic-function-lambda-list
     sb-pcl::compute-slots)
   ;;
   ;; CMU
   #+cmu
   '(pcl:class-of
     pcl:class-name
     pcl:class-slots
     pcl:find-class
     pcl::standard-clss
     pcl::slot-definition-name
     pcl::finalize-inheritance
     pcl::standard-direct-slot-definition
     pcl::standard-effective-slot-definition
     pcl::validate-superclass
     pcl::direct-slot-definition-class
     pcl::effective-slot-definition-class
     pcl:compute-effective-slot-definition
     pcl:class-direct-slots
     pcl::compute-effective-slot-definition-initargs
     pcl::slot-value-using-class
     pcl:class-prototype
     pcl:generic-function-method-class
     pcl:intern-eql-specializer
     pcl:make-method-lambda
     pcl:generic-function-lambda-list
     pcl::compute-slots)
   ;;
   ;; Scieneer Common Lisp
   #+scl
   '(class-of
     class-name
     class-slots
     find-class
     clos::standard-class
     clos::slot-definition-name
     clos::finalize-inheritance
     clos::standard-direct-slot-definition
     clos::standard-effective-slot-definition
     clos::effective-slot-definition-class
     clos:class-direct-slots
     clos::validate-superclass
     clos:direct-slot-definition-class
     clos:compute-effective-slot-definition
     clos::compute-effective-slot-definition-initargs
     clos::slot-value-using-class
     clos::class-prototype
     clos:generic-function-method-class
     clos:intern-eql-specializer
     clos:make-method-lambda
     clos:generic-function-lambda-list
     clos::compute-slots)
   ;;
   ;; OpenMCL / Closure Common Lisp
   #+openmcl
   '(openmcl-mop::slot-definition-name
     openmcl-mop:finalize-inheritance
     openmcl-mop::standard-direct-slot-definition
     openmcl-mop::standard-effective-slot-definition
     openmcl-mop::validate-superclass
     openmcl-mop:direct-slot-definition-class
     openmcl-mop::effective-slot-definition-class
     openmcl-mop:compute-effective-slot-definition
     openmcl-mop:class-direct-slots
     openmcl-mop::compute-effective-slot-definition-initargs
     openmcl-mop::slot-value-using-class
     openmcl-mop:class-prototype
     openmcl-mop:generic-function-method-class
     openmcl-mop:intern-eql-specializer
     openmcl-mop:make-method-lambda
     openmcl-mop:generic-function-lambda-list
     openmcl-mop::compute-slots)
   ;;
   ;; Symbolics Genera (Future Common Lisp)
   #+genera
   '(clos:class-of
     clos:class-name
     clos:class-slots
     clos:find-class
     clos::standard-class
     clos::slot-definition-name
     clos-internals::finalize-inheritance
     clos-internals::standard-direct-slot-definition
     clos-internals::standard-effective-slot-definition
     clos-internals::effective-slot-definition-class
     clos-internals::compute-effective-slot-definition
     clos:class-direct-slots
     clos-internals::validate-superclass
     clos-internals::direct-slot-definition-class
     clos-internals::compute-effective-slot-definition-initargs
     clos::slot-value-using-class
     clos::class-prototype
     clos::ensure-generic-function
     clos:generic-function-method-class
     clos-internals::make-method-lambda
     clos:generic-function-lambda-list
     clos-internals::compute-slots)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(class-of
            class-name
            class-slots
            find-class
            standard-class
            slot-definition-name
            finalize-inheritance
            standard-direct-slot-definition
            standard-effective-slot-definition
            validate-superclass
            compute-effective-slot-definition-initargs
            direct-slot-definition-class
            effective-slot-definition-class
            compute-effective-slot-definition
            slot-value-using-class
            class-prototype
            generic-function-method-class
            intern-eql-specializer
            make-method-lambda
            generic-function-lambda-list
            compute-slots
            class-direct-slots
            process-slot-option
            process-class-option))
  
  #+sbcl
  (if (find-package #'sb-mop)
      (setq cl:*features* (delete :cl-hacks-sbcl-mop cl:*features*))
      (setq cl:*features* (delete :cl-hacks-sbcl-pcl cl:*features*)))
  
  #+cmu
  (if (find-package #'mop)
      (setq cl:*features* (delete :cl-hacks-cmu-mop cl:*features*))
      (setq cl:*features* (delete :cl-hacks-cmu-pcl cl:*features*)))
  
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
