;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; types.lisp --- Types
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:54:06 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    26 Nov 2011 20:31:24
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
;;;{{{ Array types:

#||
Type designator for an index into array of LENGTH: an integer between
0 (inclusive) and LENGTH (exclusive). LENGTH defaults to
ARRAY-DIMENSION-LIMIT.
||#
(deftype array-index (&optional (length array-dimension-limit))
  `(integer 0 (,length)))

#||
Type designator for a dimension of an array of LENGTH: an integer between
0 (inclusive) and LENGTH (inclusive). LENGTH defaults to
ARRAY-DIMENSION-LIMIT.
||#
(deftype array-length (&optional (length array-dimension-limit))
  `(integer 0 ,length))


;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ String types:

#||
A string designator type. A string designator is either a string, a symbol,
or a character.
||#
(deftype string-designator ()
  `(or symbol string character))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ CDR5 types:

;; This MACROLET will generate most of CDR5
;; (http://cdr.eurolisp.org/document/5/)
;; except the RATIO related definitions and ARRAY-INDEX.
(macrolet
    ((frob (type &optional (base-type type))
       (let ((subtype-names (list))
             (predicate-names (list)))
         (flet ((make-subtype-name (format-control)
                  (let ((result (format-symbol :cl-hacks-internals
                                               format-control
                                               (symbol-name type))))
                    (push result subtype-names)
                    result))
                (make-predicate-name (sybtype-name)
                  (let ((result (format-symbol :cl-hacks-internals
                                               "~A-P"
                                               (symbol-name sybtype-name))))
                    (push result predicate-names)
                    result))
(make-docstring (range-beg range-end range-type)
  (let ((inf (ecase range-type (:negative "-inf") (:positive "+inf"))))
    (format nil "Type specifier denoting the ~(~A~) range from ~A to ~A."
    type
    (if (equal range-beg ''*) inf (ensure-car range-beg))
    (if (equal range-end ''*) inf (ensure-car range-end))))))
           (let* ((negative-name     (make-subtype-name "NEGATIVE-~A"))
                  (non-positive-name (make-subtype-name "NON-POSITIVE-~A"))
                  (non-negative-name (make-subtype-name "NON-NEGATIVE-~A"))
                  (positive-name     (make-subtype-name "POSITIVE-~A"))
                  (negative-p-name     (make-predicate-name negative-name))
                  (non-positive-p-name (make-predicate-name non-positive-name))
                  (non-negative-p-name (make-predicate-name non-negative-name))
                  (positive-p-name     (make-predicate-name positive-name))
                  (negative-extremum)
                  (positive-extremum)
                  (below-zero)
                  (above-zero)
                  (zero))
             (setf (values negative-extremum below-zero
                           above-zero positive-extremum zero)
                   (ecase type
                     (fixnum       (values 'most-negative-fixnum -1 1 'most-positive-fixnum 0))
                     (integer      (values ''* -1       1        ''* 0))
                     (rational     (values ''* '(0)     '(0)     ''* 0))
                     (real         (values ''* '(0)     '(0)     ''* 0))
                     (float        (values ''* '(0.0E0) '(0.0E0) ''* 0.0E0))
                     (short-float  (values ''* '(0.0S0) '(0.0S0) ''* 0.0S0))
                     (single-float (values ''* '(0.0F0) '(0.0F0) ''* 0.0F0))
                     (double-float (values ''* '(0.0D0) '(0.0D0) ''* 0.0D0))
                     (long-float   (values ''* '(0.0L0) '(0.0L0) ''* 0.0L0))))
             `(progn
                (deftype ,negative-name ()
  ,(make-docstring negative-extremum below-zero :negative)
  `(,',base-type ,,negative-extremum ,',below-zero))

                (deftype ,non-positive-name ()
  ,(make-docstring negative-extremum zero :negative)
  `(,',base-type ,,negative-extremum ,',zero))

                (deftype ,non-negative-name ()
  ,(make-docstring zero positive-extremum :positive)
  `(,',base-type ,',zero ,,positive-extremum))

                (deftype ,positive-name ()
  ,(make-docstring above-zero positive-extremum :positive)
  `(,',base-type ,',above-zero ,,positive-extremum))

                (declaim (inline ,@predicate-names))

                (defun ,negative-p-name (n)
                  (and (typep n ',type)
                       (< n ,zero)))

                (defun ,non-positive-p-name (n)
                  (and (typep n ',type)
                       (<= n ,zero)))

                (defun ,non-negative-p-name (n)
                  (and (typep n ',type)
                       (<= ,zero n)))

                (defun ,positive-p-name (n)
                  (and (typep n ',type)
                       (< ,zero n)))))))))
  (frob fixnum integer)
  (frob integer)
  (frob rational)
  (frob real)
  (frob float)
  (frob short-float)
  (frob single-float)
  (frob double-float)
  (frob long-float))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Functions:

#||
Returns a function of one argument, which returns true when its argument is
of TYPE.
||#
(defun of-type (type)
  (lambda (thing) (typep thing type)))

(define-compiler-macro of-type (&whole form type &environment env)
  ;; This can yeild a big benefit, but no point inlining the function
  ;; all over the place if TYPE is not constant.
  (if (constantp type env)
      (with-gensyms (thing)
        `(lambda (,thing)
           (typep ,thing ,type)))
      form))

#||
Returns a primary value of T is TYPE1 and TYPE2 are the same type,
and a secondary value that is true is the type equality could be reliably
determined: primary value of NIL and secondary value of T indicates that the types are not equivalent.
||#
(declaim (inline type=))
(defun type= (type1 type2)

  (multiple-value-bind (sub ok) (subtypep type1 type2)
    (cond ((and ok sub)
           (subtypep type2 type1))
          (ok
           (values nil ok))
          (t
           (multiple-value-bind (sub ok) (subtypep type2 type1)
             (declare (ignore sub))
             (values nil ok))))))

(define-modify-macro coercef (type-spec) coerce
  "Modify-macro for COERCE.")


;;;}}}
;;; ==================================================================

;;; types.lisp ends here
