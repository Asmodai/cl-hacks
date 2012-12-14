;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; definitions.lisp --- Various methods for defining things
;;;
;;; Time-stamp: <Friday Dec 14, 2012 05:40:23 asmodai>
;;; Revision:   9
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    25 Nov 2011 23:44:17
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
;;;{{{ Internal methods:

(defun %re-evaluate-constant (name value test)
  (if (not (boundp name))
      value
      (let ((old (symbol-value name))
            (new value))
        (if (not (constantp name))
            (prog1 new
              (cerror "Trying to redefine the variable as a constant."
                      "~@<~S is an already-bound non-constant ~
                       variable whose value is ~S.~:@>" name old))
            (if (funcall test old new)
                old
                (restart-case
                    (error "~@<~S is an already-defined constant ~
                            whose value ~S is not equal to the ~
                            provided initial value ~S under ~S.~:@>"
                           name old new test)
                    (ignore ()
                      :report "Retain the current value."
                      old)
                    (continue ()
                      :report "Try to redefine the constant."
                      new)))))))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Defining constants and variables:

(defmacro define-constant (name initial-value
                           &key (test ''equal) docstr)
  `(defconstant ,name (%re-evaluate-constant ',name ,initial-value
                                             ,test)
     ,@(when docstr `(,docstr))))

(defmacro defconstant* (sym value &optional docstr)
  `(defconstant ,sym (if (boundp ',sym)
                         (symbol-value ',sym)
                         ,value)
     ,@(when docstr (list docstr))))

(defmacro defcustom (name type init docstr)
  `(progn
     (declaim (type ,type ,name))
     (defvar ,name (the ,type ,init) ,docstr)))

(defmacro defconst (name type init docstr)
  `(progn
     (declaim (type ,type ,name))
     (,(if (subtypep type '(or symbol number character))
           'defconstant
           'defvar)
       ,name (the ,type ,init) ,docstr)))

(defmacro make-typed-array (type init &optional len)
  (if len
      `(make-array ,len :element-type ,type :initial-element ,init)
      `(make-array (length ,init) :element-type ,type
                   :initial-contents ,init)))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Constant predicates and evaluation:

(defun named-constant-p (name &optional env)
  env
  (when (constantp name)
    (return-from named-constant-p
      (values t (symbol-value name))))
  nil)

(defun evaluate-constant (form &optional env)
  (setf form (macroexpand form env))
  (if (atom form)
      (cond ((not (symbolp form))
             form)
            (t
             (multiple-value-bind (constant-p value)
                 (named-constant-p form env)
               (if constant-p
                   value
                   (error "Symbol ~S does not have a value."
                          form)))))
      (case (first form)
        (quote
         (second form))
        (values
         (apply #'values
                (loop for value in (cdr form)
                      collect (evaluate-constant value env))))
        (otherwise
         (error "Constant evaluator called on ~S" form)))))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Parsing:

(defmacro discard-docstring (body-var &optional force-p)
  `(when (and (stringp (car ,body-var))
              (or ,force-p (cdr ,body-var)))
     (pop ,body-var)))

(defun parse-body (body &key documentation whole)
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
      :declarations
      (setf current (car body))
      (when (and documentation
                 (stringp current)
                 (cdr body))
        (if doc
            (error "Too many documentation strings in ~S."
                   (or whole body))
            (setf doc (pop body)))
        (go :declarations))
      (when (and (listp current)
                 (eql (first current) 'declare))
        (push (pop body) decls)
        (go :declarations)))
    (values body (nreverse decls) doc)))

(defun parse-ordinary-lambda-list (lambda-list)
  (let ((state :required)
        (allow-other-keys nil)
        (auxp nil)
        (required nil)
        (optional nil)
        (rest nil)
        (keys nil)
        (aux nil))
    (labels ((fail (elt)
               (simple-program-error
                "Misplaced ~S in ordinary lambda-list:~%  ~S"
                elt lambda-list))
             (check-variable (elt what)
               (unless (and (symbolp elt) (not (constantp elt)))
                 (simple-program-error
                  "Invalid ~A ~S in ordinary lambda-list:~%  ~S"
                  what elt lambda-list)))
             (check-spec (spec what)
               (destructuring-bind (init suppliedp) spec
                 (declare (ignore init))
                 (check-variable suppliedp what))))
      (dolist (elt lambda-list)
        (case elt
          (&optional
           (if (eq state :required)
               (setf state elt)
               (fail elt)))
          (&rest
           (if (member state '(:required &optional))
               (setf state elt)
               (progn
                 (break "state=~S" state)
                 (fail elt))))
          (&key
           (if (member state '(:required &optional :after-rest))
               (setf state elt)
               (fail elt)))
          (&allow-other-keys
           (if (eq state '&key)
               (setf allow-other-keys t
                     state elt)
               (fail elt)))
          (&aux
           (cond ((eq state '&rest)
                  (fail elt))
                 (auxp
                  (simple-program-error
                   "Multiple ~S in ordinary lambda-list:~%  ~S"
                   elt lambda-list))
                 (t
                  (setf auxp t
                        state elt))
                 ))
          (otherwise
           (when (member
                  elt
                  '#.(set-difference
                      lambda-list-keywords
                      '(&optional &rest &key &allow-other-keys &aux)))
             (simple-program-error
              "Bad lambda-list keyword ~S in ordinary lambda-list:~% ~S"
              elt lambda-list))
           (case state
             (:required
              (check-variable elt "required parameter")
              (push elt required))
             (&optional
              (cond ((consp elt)
                     (destructuring-bind (name &rest tail) elt
                       (check-variable name "optional parameter")
                       (if (cdr tail)
                           (check-spec tail "optional-supplied-p parameter")
                           (setf elt (append elt '(nil))))))
                    (t
                     (check-variable elt "optional parameter")
                     (setf elt (cons elt '(nil nil)))))
              (push elt optional))
             (&rest
              (check-variable elt "rest parameter")
              (setf rest elt
                    state :after-rest))
             (&key
              (cond
                ((consp elt)
                 (destructuring-bind (var-or-kv &rest tail) elt
                   (cond ((consp var-or-kv)
                          (destructuring-bind (keyword var) var-or-kv
                            (unless (symbolp keyword)
                              (simple-program-error
                               "Invalid keyword name ~S in ordinary ~
                                lambda-list:~%  ~S"
                               keyword lambda-list))
                            (check-variable var "keyword parameter")))
                         (t
                          (check-variable var-or-kv "keyword parameter")
                          (setf var-or-kv (list
                                           (make-keyword var-or-kv)
                                           var-or-kv))))
                   (if (cdr tail)
                       (check-spec tail "keyword-supplied-p parameter")
                       (setf tail (append tail '(nil))))
                   (setf elt (cons var-or-kv tail))))
                (t
                 (check-variable elt "keyword parameter")
                 (setf elt (list (list (make-keyword elt) elt) nil nil))))
              (push elt keys))
             (&aux
              (if (consp elt)
                  (destructuring-bind (var &optional init) elt
                    (declare (ignore init))
                    (check-variable var "&aux parameter"))
                  (check-variable elt "&aux parameter"))
              (push elt aux))
             (t
              (simple-program-error
               "Invalid ordinary lambda-list:~%  ~S"
               lambda-list)))))))
    (values (nreverse required) (nreverse optional)
            rest (nreverse keys)
            allow-other-keys (nreverse aux))))

;;;}}}
;;; ==================================================================

;;; definitions.lisp ends here
