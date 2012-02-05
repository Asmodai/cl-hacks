;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; features.lisp --- Feature testing
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:50:44 asmodai>
;;; Revision:   13
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    26 Nov 2011 20:12:44
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

(defun featurep (feature-expr)
  (etypecase feature-expr
    (symbol
     (not (null (member feature-expr cl:*features*))))
    (cons
     (check-type (first feature-expr) symbol)
     (eswitch ((first feature-expr) :test 'string=)
       (:and
        (every #'featurep (rest feature-expr)))
       (:or
        (some #'featurep (rest feature-expr)))
       (:not
        (assert (= 2 (length feature-expr)))
        (not (featurep (second feature-expr))))))))

;;; ==================================================================
;;;{{{ Feature standardisation:

;;; ------------------------------------------------------------------
;;;{{{ ABCL:

#+abcl
(progn
  
  (pushnew (let ((order (jcall
                         "toString"
                         (jstatic "nativeOrder"
                                  "java.nio.ByteOrder"))))
             (cond ((string-equal order "LITTLE_ENDIAN")
                    :little-endian)
                   ((string-equal order "BIG_ENDIAN")
                    :big-endian)
                   (t
                    (error "Byte order ~A is unknown." order))))
           cl:*features*)
  
  ) ;; progn ends here
  

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Allegro:

#+allegro
(progn
  
  (cond ((featurep :mswindows)
         (pushnew :windows cl:*features*))
        ((featurep :macosx)
         (pushnew :darwin cl:*features*))
        ((featurep '(:or :macosx :darwin :freebsd :netbsd :openbsd))
         (pushnew :bsd cl:*features*))
        ((featurep :powerpc)
         (pushnew :ppc cl:*features*)))

  ) ;; progn ends here

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ CLISP:

#+clisp
(progn

  (pushnew (intern (symbol-name (if sys::*big-endian*
                                    '#:big-endian
                                    '#:little-endian))
                   '#:keyword)
           cl:*features*)

  (cond ((featurep :win32)
         (pushnew :windows cl:*features*))
        ((not (featurep :win32))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (pushnew (with-standard-io-syntax
                      (read-from-string
                       (format nil ":~(~A~)"
                               (posix:uname-sysname (posix:uname)))))
                    cl:*features*))))

  (when (featurep (:or :darwin :freebsd :netbsd :openbsd))
    (pushnew :bsd cl:*features*))

  (pushnew (intern
            (symbol-name
             (cond ((string= (machine-type) "X86_64")
                    '#:x86-64)
                   ((member :pc386 cl:*features*)
                    '#:x86)
                   ((string= (machine-type) "POWER MACINTOSH")
                    '#:ppc)))
            '#:keyword)
           cl:*features*)

  ) ;; progn ends here

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ CMU:

#+cmu
(progn

  (pushnew
   (alien:with-alien ((ptr (array (alien:unsigned 8) 2)))
     (setf (sys:sap-ref-16 (alien:alien-sap ptr) 0) #xfeff)
     (ecase (sys:sap-ref-8 (alien:alien-sap ptr) 0)
       (#xfe (intern (symbol-name '#:big-endian) '#:keyword))
       (#xff (intern (symbol-name '#:little-endian) '#:keyword))))
   cl:*features*)

  ) ;; progn ends here

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Corman:

#+cormanlisp
(progn

  (pushnew :little-endian cl:*features*)
  (pushnew :windows cl:*features*)
  (pushnew :x86 cl:*features*)

  ) ;; progn ends here

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ ECL:

#+ecl
(progn

  (pushnew
   (let ((ptr (ffi:allocate-foreign-object :unsigned-short)))
     (unwind-protect
         (progn
           (setf (ffi:deref-pointer ptr :unsigned-short) #xfeff)
           (ecase (ffi:deref-pointer ptr :unsigned-byte)
             (#xfe (intern (symbol-name '#:big-endian) '#:keyword))
             (#xff (intern (symbol-name '#:little-endian)
                           '#:keyword))))
       (ffi:free-foreign-object ptr)))
   cl:*features*)

  (cond ((featurep :darwin)
         (pushnew :unix cl:*features*))
        ((featurep :win32)
         (pushnew :windows cl:*features*)))

  (cond ((featurep :powerpc7450)
         (pushnew :ppc cl:*features*))
        ((featurep :x86_64)
         (pushnew :x86-64 cl:*features*))
        ((featurep (:or :i386 :i486 :i586 :i686))
         (pushnew :x86 cl:*features*)))

  ) ;; progn ends here

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ LispWorks:

#+lispworks
(progn

  (if (not (featurep :little-endian))
      (pushnew :big-endian cl:*features*))

  #-(and)
  (pushnew
   (fli:with-dynamic-foreign-objects ()
     (let ((ptr (fli:alloca :type :byte :nelems 2)))
       (setf (fli:dereference ptr :type '(:unsigned :short)) #xfeff)
       (ecase (fli:dereference ptr :type '(:unsigned :byte))
         (#xfe (intern (symbol-name '#:big-endian) '#:keyword))
         (#xff (intern (symbol-name '#:little-endian) '#:keyword)))))
   cl:*features*)

  (if (featurep :win32)
      (pushnew :windows cl:*features*))

  (if (featurep '(:or :darwin :freebsd :netbsd :openbsd))
      (pushnew :bsd cl:*features*))

  (if (featurep :powerpc)
      (pushnew :ppc cl:*features*))

  ) ;; progn ends here

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Closure/OpenMCL/MCL:

#+(or :ccl :openmcl :mcl)
(progn

  (if (featurep :big-endian-target)
      (pushnew :big-endian cl:*features*))

  (if (featurep :little-endian-target)
      (pushnew :little-endian cl:*features*))

  (if (featurep :linux-target)
      (pushnew :linux cl:*features*))

  (if (featurep :darwin)
      (pushnew :bsd cl:*features*))

  (if (featurep :ppc-target)
      (pushnew :ppc cl:*features*))

  (if (featurep :x8664-target)
      (pushnew :x86-64 cl:*features*))

  ) ;; progn ends here

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ SBCL:

#+sbcl
(progn  

  (pushnew
   (sb-alien:with-alien ((ptr (array (sb-alien:unsigned 8) 2)))
     (setf (sb-sys:sap-ref-16 (sb-alien:alien-sap ptr) 0) #xfeff)
     (ecase (sb-sys:sap-ref-8 (sb-alien:alien-sap ptr) 0)
       (#xfe (intern (symbol-name '#:big-endian) '#:keyword))
       (#xff (intern (symbol-name '#:little-endian) '#:keyword))))
   cl:*features*)

  (if (featurep :win32)
      (progn
        (setq cl:*features* (remove :unix cl:*features*))
        (pushnew :windows cl:*features*)))

  ) ;; progn ends here

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ SCL:

#+scl
(progn

  (pushnew
   (align:with-alien ((ptr (array (alien:unsigned 8) 2)))
     (setf (sys:sap-ref-16 (alien:alien-sap ptr) 0) #xfeff)
     (ecase (sys:sap-ref-8 (alien:alien-sap ptr) 0)
       (#xfe (intern (symbol-name '#:big-endian) '#:keyword))
       (#xff (intern (symbol-name '#:little-endian) '#:keyword))))
   cl:*features*)

  (if (featurep :amd64)
      (pushnew :x86-64 cl:*features*))

  ) ;; progn ends here

;;;}}}
;;; ------------------------------------------------------------------

;;;}}}
;;; ==================================================================

;;; features.lisp ends here
