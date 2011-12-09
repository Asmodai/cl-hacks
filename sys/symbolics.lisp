;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; symbolics.lisp --- Functionality taken from Zetalisp and Symbolics Common Lisp
;;;
;;; Time-stamp: <Friday Dec  9, 2011 09:40:26 asmodai>
;;; Revision:   29
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 15:41:02
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
;;; This file is a collection of `stuff' that is taken from both
;;; Zetalisp and Symbolics Common Lisp.
;;;
;;; Most of the SCL stuff is stuff that never made it into the Common
;;; Lisp specification, but is still somewhat handy to have.
;;;
;;; }}}

#-genera
(in-package #:cl-hacks-internals)

;;; ==================================================================
;;; {{{ Stuff that needs to load first:

(eval-when (:load-toplevel :compile-toplevel :execute)

  ;; ZL:MEM
  (defun mem (pred item list)
    (do ((l list (cdr l)))
        (nil l)
      (and (funcall pred item (car l))
           (return l))))
  
  ;; ZL:REST1
  (proclaim '(inline rest1))
  (defun rest1 (list)
    (nthcdr 1 list))
  
  ;; ZL:SELECTQ
  (defmacro selectq (test-object &body clauses)
    `(case ,test-object
       ,@(mapcar #'(lambda (clause)
                     `(,(let ((key (first clause)))
                          (if (null key)
                              '(nil)
                              key))
                        ,@(rest1 clause)))
                 clauses)))

  ;; CLI:COERCE-STRING-ARG
  (defmacro coerce-string-arg (arg-name &optional destructive)
    `(if ,destructive
         (cl:check-type ,arg-name string)
         (unless (stringp ,arg-name)
           (setq ,arg-name (string ,arg-name)))))
  
  ;; SCL:STRING-APPEND
  (defun string-append (&rest strings &aux string)
    (declare (ignorable string))
    (apply #'concatenate
           'string
           (loop for s in strings
                 collect (progn
                           (coerce-string-arg s)
                           s))))

  ;; SYS:FIND-BODY-DECLARATIONS
  (defun find-body-declarations (body env &optional
                                 (lambda-list nil lambda-list-p))
    (declare (ignore lambda-list))
    (loop for real-body on body
          for form = (first real-body)
          do (when (and (listp form)
                        (not (eq (first form) 'declare)))
               (setf form (macroexpand form env)))
          while (cond ((stringp form)
                       (and lambda-list-p
                            (not (null (rest1 real-body)))))
                      ((listp form)
                       (eq (first form) 'declare)))
          collect form into declarations
          finally (return (values declarations real-body
                                  (and real-body form)))))

  ) ;; (eval-when ...)

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Internal functions:

;;; Kludgy... we ignore the Lisp Machine stuff and hope that the CL
;;; implementation can optimize it :)
(defun %string-equal (string1 index1 string2 index2 count)
  (cond ((if (null count)
             (/= (setq count (- (length string1) index1))
                 (- (length string2) index2))
             (or (> (+ index1 count) (length string1))
                 (> (+ index2 count) (length string2))))
         nil)
        (t
         (let ((string1 string1)
               (string2 string2))
           (do ()
               ((zerop count) t)
             (unless (char-equal (aref string1 index1)
                                 (aref string2 index2))
               (return nil))
             (incf index1)
             (incf index2)
             (decf count))))))

;;; Internal version of `string=' that acts similar to the Zetalisp
;;; `string=' function.
(defun %string= (string1 index1 string2 index2 count)
  (cond ((if (null count)
             (/= (setq count (- (length string1) index1))
                 (- (length string2) index2))
             (or (> (+ index1 count) (length string1))
                 (> (+ index2 count) (length string2))))
         nil)
        (t
         (let ((string1 string1)
               (string2 string2))
           (do ()
               ((zerop count) t)
             (unless (char= (aref string1 index1)
                            (aref string2 index2))
               (return nil))
             (incf index1)
             (incf index2)
             (decf count))))))

(defun %string-compare (string1 index1 string2 index2 count)
  (let ((string1 string1)
        (string2 string2))
    (let ((subrange1 (- (length string1) index1))
          (subrange2 (- (length string2) index2))
          (status 0))
      (cond ((null count)
             (setq count (min subrange1 subrange2)
                   status 1))
            ((> count subrange1)
             (cond ((> count subrange2)
                    (setq count (min subrange1 subrange2)
                          status 1))
                   (t
                    (setq count subrange1
                          status 2))))
            ((> count subrange2)
             (setq count subrange2
                   status 3)))
      (do ((i1 index1 (1+ i1))
           (i2 index2 (1+ i2))
           char1 char2)
          ((zerop count)
           (selectq status
                    (0 0)
                    (1 (cond ((= subrange1 subrange2)
                              0)
                             ((< subrange1 subrange2)
                              (- -1 i1))
                             (t
                              (1+ i1))))
                    (2
                     (- index1 i1 1))
                    (t
                     (1+ (- i1 index1)))))
        (setq char1 (aref string1 i1)
              char2 (aref string2 i2))
        (or (char-equal char1 char2)
            (return (if (char-lessp char1 char2)
                        (- index1 i1 1)
                        (- (1+ i1) index1))))
        (decf count)))))

(defun %string-exact-compare (string1 index1 string2 index2 count)
  (let ((string1 string1)
        (string2 string2))
    (let* ((subrange1 (- (length string1) index1))
           (subrange2 (- (length string2) index2))
           (status 0))
      (cond ((null count)
             (setq count (min subrange1 subrange2)
                   status 1))
            ((> count subrange1)
             (cond ((> count subrange2)
                    (setq count (min subrange1 subrange2)
                          status 1))
                   (t
                    (setq count subrange1
                          status 2))))
            ((> count subrange2)
             (setq count subrange2
                   status 3)))
      (do ((i1 index1 (1+ i1))
           (i2 index2 (1+ i2))
           char1 char2)
          ((zerop count)
           (selectq status
                    (0 0)
                    (1 (cond ((= subrange1 subrange2)
                              0)
                             ((< subrange1 subrange2)
                              (- -1 i1))
                             (t
                              (1+ i1))))
                    (2
                     (- index1 i1 1))
                    (t
                     (1+ (- i1 index1)))))
        (setq char1 (aref string1 i1)
              char2 (aref string2 i2))
        (unless (char= char1 char2)
          (return (if (char< char1 char2)
                      (- index1 i1 1)
                      (- (1+ i1) index1))))
        (decf count)))))

(defun %string-search-char (char string start end)
  (let ((str string))
    (do ((i start (1+ i)))
        ((>= i end) nil)
      (when (char-equal (aref str i) char)
        (return i)))))

(defun %string-search-exact-char (char string start end)
  (let ((str string))
    (do ((i start (1+ i)))
        ((>= i end) nil)
      (when (char= (aref str i) char)
        (return i)))))

(defun %string-search-array-set (string start end set)
  (let ((string string)
        (set set)
        (set-length (length set)))
    (do ((i start (1+ i)))
        ((>= i end) nil)
      (let ((char (aref string i)))
        (do ((j 0 (1+ j)))
            ((>= j set-length) nil)
          (when (char-equal char (aref set j))
            (return-from %string-search-array-set i)))))))

(defun %string-search-array-set-reverse (string start end set)
  (let ((string string)
        (set set)
        (set-length (length set)))
    (do ((i (1- end) (1- i)))
        ((< i start) nil)
      (let ((char (aref string i)))
        (do ((j 0 (1+ j)))
            ((>= j set-length) nil)
          (when (char-equal char (aref set j))
            (return-from %string-search-array-set-reverse i)))))))

(defun %string-search-not-array-set (string start end set)
  (let ((string string)
        (set set)
        (set-length (length set)))
    (do ((i start (1+ i)))
        ((>= i end) nil)
      (let ((char (aref string i)))
        (do ((j 0 (1+ j)))
            ((>= j set-length)
             (return-from %string-search-not-array-set i))
          (when (char-equal char (aref set j))
            (return)))))))

(defun %string-search-not-array-set-reverse (string start end set)
  (let ((string string)
        (set set)
        (set-length (length set)))
    (do ((i (1- end) (1- i)))
        ((< i start) nil)
      (let ((char (aref string i)))
        (do ((j 0 (1+ j)))
            ((>= j set-length)
             (return-from %string-search-not-array-set-reverse i))
          (when (char-equal char (aref set j))
            (return)))))))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Zetalisp:

;;; SI:NEQ
(defsubst neq (x y)
  (not (eq x y)))

;;; Taken from ZL:NAMED-LAMBDA
(defmacro named-lambda (name lambda-list &body body)
  (declare (ignore name))
  `(lambda ,lambda-list ,@body))

;;; SI:ARRAY-MEM
(defun array-mem (function item array)
  (dotimes (i (length array))
    (when (funcall function item (aref array i))
      (return t))))

;;; SYS:VALIDATE-FUNCTION-SPEC
(defun validate-function-spec (fspec &optional nil-allowed
                               &aux handler)
  (declare (ignorable handler))
  (cond ((null fspec)
         nil-allowed)
        ((symbolp fspec)
         t)
        ((atom fspec)
         nil)))

;;; SYS:STANDARDIZE-FUNCTION-SPEC
(defun standardize-function-spec (fspec &optional (error-p t))
  (and (listp fspec)
       (= (length fspec) 2)
       (symbolp (car fspec)))
  (unless (validate-function-spec fspec)
    (if error-p
        (error "~S is not a valid function spec." fspec)
        (return-from standardize-function-spec nil)))
  fspec)

;;; LT:NAMED-CONSTANT-P
(defun named-constant-p (name &optional env)
  env
  (when (constantp name)
    (return-from named-constant-p
      (values t (symbol-value name))))
  nil)

;;; LT:EVALUATE-CONSTANT
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

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Symbolics Common Lisp:

;;; SI:DEFPROP
(defmacro defprop (sym value indicator)
  (when (not (symbolp sym))
    (error "~S is not a symbol." sym))
  `(setf (get ',sym ',indicator) ',value))

;;; SCL:DEFSELECT
(defmacro defselect (fspec &body methods)
  (let* ((default-handler (if (consp fspec) (second fspec) nil))
         (no-which-operations (if (consp fspec) (third fspec) nil))
         (fspec (standardize-function-spec (if (consp fspec)
                                               (first fspec)
                                               fspec)))
         (operation-list nil)
         (clauses-list nil))
    (loop for (key . method-body) in methods
          doing (if (consp key)
                    (setq operation-list (revappend key operation-list))
                    (push key operation-list))
          doing (if (symbolp method-body)
                    (push `(,key (apply #',method-body op args))
                          clauses-list)
                    (push `(,key (apply (lambda ,@method-body) args))
                          clauses-list)))
    (cond (no-which-operations
           (setq operation-list (nreverse operation-list)))
          (:else
           (setq clauses-list
                 (append `((:which-operations
                            (apply (lambda (&rest ignore)
                                     ',operation-list) args))
                           (:operation-handled-p
                            (apply (lambda (op &rest ignore)
                                     (not (null (member op
                                                        ',operation-list
                                                        :test #'eq))))
                                   args))
                           (:send-if-handles
                            (apply (lambda (op &rest to-send)
                                     (when (member op ',operation-list
                                                   :test #'eq)
                                       (apply (function ,fspec) op
                                              to-send)))
                                   args)))
                         clauses-list))))
    (setq clauses-list (nreverse clauses-list))
    `(defun ,fspec (op &rest args)
       ,(if default-handler
            `(case op
               ,@clauses-list
               (otherwise (apply #',default-handler op args)))
            `(ccase op
               ,@clauses-list)))))

;;; SCL:DEFSUBST
(defmacro defsubst (name lambda-list &body body)
  `(progn
     (proclaim '(inline ,name))
     (defun ,name ,lambda-list ,@body)))

;;; SCL:STRING-LENGTH
(defun string-length (string)
  (cond ((arrayp string)
         (length string))
        ((characterp string)
         1)
        ((symbolp string)
         (length (string string)))
        ((or (typep string 'string)
             (typep string 'simple-string))
         (length string))
        (t
         (error "Cannot coerce ~S into a string." string))))

;;; SCL:SUBSTRING
(defmacro substring (string from &optional to (area nil))
  (declare (ignore area))
  `(subseq ,string ,from ,to))

;;; SCL:NSUBSTRING
;;; TODO: This uses index-offset arrays on Genera...
(defmacro nsubstring (string from &optional to (area nil)
                      &aux length arraytype)
  (declare (ignore area length arraytype))
  `(subseq ,string ,from ,to))

;;; SCL:CHAR-FLIPCASE
(defun char-flipcase (char)
  (if (upper-case-p char)
      (char-downcase char)
      (char-upcase char)))

;;; ------------------------------------------------------------------
;;; {{{ String functions that take keywords:

;;;
;;; Symbolics Common Lisp provides optimization for these macros... we
;;; don't, however.
;;;
;;; This code is meant to be portable, and I do not feel that spending
;;; bignum cycles investigating how to optimize these routines on
;;; fixnum Common Lisp implementations is worth the effort.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *string-macro-package* *package*))

;;; Defines a function that operates on a string
(defmacro define-string-keyword-fn (name requireds keywords &body body)
  (let ((keyword-symbols (loop for (k) in keywords
                               collect k))
        (name-internal (intern (string-append name "-INTERNAL")
                               *string-macro-package*)))
    (multiple-value-bind (declarations real-body)
        (find-body-declarations body nil requireds)
      `(progn
         (defun ,name (,@requireds &key . ,keywords)
           ,@declarations
           (,name-internal ,@requireds . ,keyword-symbols))
         (defun ,name-internal (,@requireds . ,keyword-symbols)
           ,@declarations
           ,@real-body)))))

;;; SCL:STRING-FLIPCASE
(define-string-keyword-fn string-flipcase (string)
    ((start 0) (end nil))
  (coerce-string-arg string)
  (nstring-flipcase (string-append string) :start start :end end))

;;; SCL:NSTRING-FLIPCASE
(define-string-keyword-fn nstring-flipcase (string)
    ((start 0) (end nil))
  (coerce-string-arg string t)
  (do ((len (or end (length string)))
       (i start (1+ i)))
      ((= i len))
    (let* ((char (aref string i))
           (newchar (char-flipcase char)))
      (unless (char= char newchar)
        (setf (aref string i) newchar))))
  string)

;;; SCL:STRING-CAPITALIZE-WORDS
(define-string-keyword-fn string-capitalize-words (string)
    ((start 0) (end nil))
  (coerce-string-arg string)
  (let ((copied-string (string-append string)))
    (nstring-capitalize-words copied-string :start start :end end)
    copied-string))

;;; SCL:NSTRING-CAPITALIZE-WORDS
(define-string-keyword-fn nstring-capitalize-words (string)
  ((start 0) (end nil))
  (coerce-string-arg string t)
  (let ((state t))
    (loop for i from start below (or end (length string))
          do (let* ((char (aref string i)))
               (cond ((char-equal char #\-)
                      (setf (aref string i) #\Space)
                      (setq state t))
                     ((char-equal char #\Space)
                      (setq state t))
                     (state
                      (when (alpha-char-p char)
                        (setf (aref string i) (char-upcase char)))
                      (setq state nil))
                     ((alpha-char-p char)
                      (setf (aref string i) (char-downcase char))))))
    string))

;;; SCL:STRING-COMPARE
(define-string-keyword-fn string-compare (string1 string2)
    ((start1 0) (start2 0) (end1 nil) (end2 nil))
  (coerce-string-arg string1)
  (coerce-string-arg string2)
  (unless end1
    (setq end1 (length string1)))
  (unless end2
    (setq end2 (length string2)))
  (let* ((len2 (- end2 start2))
         (len1 (- end1 start1))
         (result (%string-compare string1 start1 string2 start2
                                  (min len1 len2))))
    (cond ((zerop result)
           (cond ((= len1 len2)
                  0)
                 ((> len1 len2)
                  (+ (+ end2 1) (- start1 start2)))
                 (t
                  (- (+ end1 1)))))
          ((plusp result)
           (+ start1 result))
          (t
           (- result start1)))))

;;; SCL:STRING-EXACT-COMPARE
(define-string-keyword-fn string-exact-compare (string1 string2)
    ((start1 0) (start2 0) (end1 nil) (end2 nil))
  (coerce-string-arg string1)
  (coerce-string-arg string2)
  (unless end1
    (setq end1 (length string1)))
  (unless end2
    (setq end2 (length string2)))
  (let* ((len2 (- end2 start2))
         (len1 (- end1 start1))
         (result (%string-exact-compare string1 start1 string2 start2
                                        (min len1 len2))))
    (cond ((zerop result)
           (cond ((= len1 len2)
                  0)
                 ((> len1 len2)
                  (+ (+ end2 1) (- start1 start2)))
                 (t
                  (- (+ end1 1)))))
          ((plusp result)
            (+ start1 result))
          (t
           (- result start1)))))

;;; SCL:STRING-REVERSE
(define-string-keyword-fn string-reverse (string) 
    ((start 0) (end nil))
  (coerce-string-arg string)
  (string-nreverse (string-append string) :start start :end end))

;;; SCL:STRING-NREVERSE
(define-string-keyword-fn string-nreverse (string)
    ((start 0) (end nil))
  (coerce-string-arg string t)
  (loop for i from start
        for j downfrom (1- (or end (length string)))
        until (<= j i)
          do (rotatef (aref string j)
                      (aref string i)))
  string)

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ String searching functions:

;;;
;;; Symbolics Common Lisp provides optimization for these macros... we
;;; don't, however.
;;;
;;; This code is meant to be portable, and I do not feel that spending
;;; bignum cycles investigating how to optimize these routines on
;;; fixnum Common Lisp implementations is worth the effort.
;;;

(defmacro define-string-search-fn (name requireds keywords
                                   forward-body backward-body)
  (let ((keyword-symbols (loop for (k) in keywords
                               collect k))
        (forward-name (intern (string-append name "-FORWARD")
                              *string-macro-package*))
        (backward-name (intern (string-append name "-BACKWARD")
                               *string-macro-package*)))
    `(progn
       (defun ,name (,@requireds &key from-end . ,keywords)
         (if from-end
             (,backward-name ,@requireds . ,keyword-symbols)
             (,forward-name ,@requireds . ,keyword-symbols)))
       (defun ,forward-name (,@requireds &optional . ,keyword-symbols)
         ,forward-body)
       (defun ,backward-name (,@requireds &optional . ,keyword-symbols)
         ,backward-body))))

;;; SCL:STRING-SEARCH-CHAR
(define-string-search-fn string-search-char (char string)
    ((start 0) (end nil))
  (progn
    (check-type char character)
    (coerce-string-arg string)
    (unless end
      (setq end (length string)))
    (%string-search-char char string start end))
  (progn
    (check-type char character)
    (coerce-string-arg string)
    (unless end
      (setq end (length string)))
    (let ((string string))
      (do ((i (1- end) (1- i)))
          ((< i start) nil)
        (when (char-equal char (aref string i))
          (return i))))))

;;; SCL:STRING-SEARCH-EXACT-CHAR
(define-string-search-fn string-search-exact-char (char string)
    ((start 0) (end nil))
  (progn
    (check-type char character)
    (coerce-string-arg string)
    (unless end
      (setq end (length string)))
    (%string-search-exact-char char string start end))
  (progn
    (check-type char character)
    (coerce-string-arg string)
    (unless end
      (setq end (length string)))
    (let ((string string))
      (do ((i (1- end) (1- i)))
          ((< i start) nil)
        (when (char= char (aref string i))
          (return i))))))

;;; SCL:STRING-SEARCH-NOT-CHAR
(define-string-search-fn string-search-not-char (char string)
    ((start 0) (end nil))
  (progn
    (check-type char character)
    (coerce-string-arg string)
    (unless end
      (setq end (length string)))
    (let ((string string))
      (do ((i start (1+ i)))
          ((>= i end) nil)
        (unless (char-equal char (aref string i))
          (return i)))))
  (progn
    (check-type char character)
    (coerce-string-arg string)
    (unless end
      (setq end (length string)))
    (let ((string string))
      (do ((i (1- end) (1- i)))
          ((< i start) nil)
        (unless (char-equal char (aref string i))
          (return i))))))

;;; SCL:STRING-SEARCH-NOT-EXACT-CHAR
(define-string-search-fn string-search-not-exact-char (char string)
    ((start 0) (end nil))
  (progn
    (check-type char character)
    (coerce-string-arg string)
    (unless end
      (setq end (length string)))
    (let ((string string))
      (do ((i start (1+ i)))
          ((>= i end) nil)
        (unless (char= char (aref string i))
          (return i)))))
  (progn
    (check-type char character)
    (coerce-string-arg string)
    (unless end
      (setq end (length string)))
    (let ((string string))
      (do ((i (1- end) (1- i)))
          ((< i start) nil)
        (unless (char= char (aref string i))
          (return i))))))

;;; SCL:STRING-SEARCH-SET
(define-string-search-fn string-search-set (char-set string)
    ((start 0) (end nil))
  (progn
    (coerce-string-arg string)
    (unless end
      (setq end (length string)))
    (if (arrayp char-set)
        (%string-search-array-set string start end char-set)
        (let ((string string))
          (do ((i start (1+ i)))
              ((>= i end) nil)
            (when (find (aref string i) char-set :test #'char-equal)
              (return i))))))
  (progn
    (coerce-string-arg string)
    (unless end
      (setq end (length string)))
    (if (arrayp char-set)
        (%string-search-array-set-reverse string start end char-set)
        (let ((string string))
          (do ((i (1- end) (1- i)))
              ((< i start) nil)
            (when (find (aref string i) char-set :test #'char-equal)
              (return i)))))))

;;; SCL:STRING-SEARCH-NOT-SET
(define-string-search-fn string-search-not-set (char-set string)
    ((start 0) (end nil))
  (progn
    (coerce-string-arg string)
    (unless end
      (setq end (length string)))
    (if (arrayp char-set)
        (%string-search-not-array-set string start end char-set)
        (do ((i start (1+ i)))
            ((>= i end) nil)
          (unless (find (aref string i) char-set :test #'char-equal)
            (return i)))))
  (progn
    (coerce-string-arg string)
    (unless end
      (setq end (length string)))
    (if (arrayp char-set)
        (%string-search-not-array-set-reverse string start end char-set)
        (do ((i (1- end) (1- i)))
            ((< i start) nil)
          (unless (find (aref string i) char-set :test #'char-equal)
            (return i))))))

;;; SCL:STRING-SEARCH
(define-string-search-fn string-search (key string)
    ((start1 0) (end1 nil) (start2 0) (end2 nil))
  (progn
    (coerce-string-arg string)
    (coerce-string-arg key)
    (unless end1
      (setq end1 (length key)))
    (unless end2
      (setq end2 (length string)))
    (let ((key-len (- end1 start1)))
      (cond ((zerop key-len)
             (and (<= start2 end2) start2))
            (t
             (decf end2 (1- key-len))
             (when (>= end2 0)
               (loop with ch1 = (aref key start1)
                     do (unless
                            (setq start2
                                  (%string-search-char ch1
                                                       string
                                                       start2
                                                       end2))
                          (return nil))
                        (and (%string-equal key start1
                                     string start2
                                     key-len)
                             (return start2))
                        (incf start2)))))))
  (progn
    (coerce-string-arg string)
    (coerce-string-arg key)
    (unless end1
      (setq end1 (length key)))
    (unless end2
      (setq end2 (length string)))
    (let ((key-len (- end1 start1)))
      (incf start2 (- key-len 1))
      (cond ((zerop key-len)
             (and (<= start2 end2) end2))
            (t
             (loop with ch1 = (aref key (1- end1))
                   for i from (1- end2) downto start2
                   do (when (and (char-equal (aref string i) ch1)
                                 (%string-equal key start1
                                                string
                                                (1+ (- i key-len))
                                                key-len))
                        (return (1+ (- i key-len))))))))))

;;; SCL:STRING-SEARCH-EXACT
(define-string-search-fn string-search-exact (key string)
    ((start1 0) (end1 nil) (start2 0) (end2 nil))
  (progn
    (coerce-string-arg string)
    (coerce-string-arg key)
    (unless end1
      (setq end1 (length key)))
    (unless end2
      (setq end2 (length string)))
    (let ((key-len (- end1 start1)))
      (cond ((zerop key-len)
             (and (<= start2 end2) start2))
            (t
             (decf end2 (1- key-len))
             (when (>= end2 0)
               (loop with ch1 = (aref key start1)
                     do (unless
                            (setq start2
                                  (%string-search-exact-char ch1
                                                             string
                                                             start2
                                                             end2))
                          (return nil))
                        (and (%string= key start1
                                     string start2
                                     key-len)
                             (return start2))
                        (incf start2)))))))
  (progn
    (coerce-string-arg string)
    (coerce-string-arg key)
    (unless end1
      (setq end1 (length key)))
    (unless end2
      (setq end2 (length string)))
    (let ((key-len (- end1 start1)))
      (incf start2 (- key-len 1))
      (cond ((zerop key-len)
             (and (<= start2 end2) end2))
            (t
             (loop with ch1 = (aref key (1- end1))
                   for i from (1- end2) downto start2
                   do (when (and (char= (aref string i) ch1)
                                 (%string= key
                                           start1
                                           string
                                           (1+ (- i key-len))
                                           key-len))
                        (return (1+ (- i key-len))))))))))

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ String pluralisation:

(defvar *compound-separators*
  '(;; Prepositions
    "about" "above" "after" "amidst" "around" "at"
    "before" "behind" "below" "beneath" "beside" "between" "by"
    "for" "from"
    "in" "inside" "into"
    "like"
    "of" "on" "onto" "outside" "over"
    "through" "to"
    "under"
    "via"
    "with"
    ;; Conjunctions
    "and" "or" "nor"))

;;; Whitespace characters.
(defvar *whitespace* '(#\Space #\Tab #\Return #\NewLine))

(defun string-pluralize-1 (string continuation)
  (let* ((pos0 (or (string-search-set *whitespace* string)
                   (length string)))
         (pos1 0)
         (pos2 0))
    (loop
      (unless pos1
        (return (string-pluralize-2 string continuation)))
      (setq pos1 (or (string-search-not-set *whitespace* string
                                            :start pos0)
                     pos0))
      (setq pos2 (or (string-search-set *whitespace* string
                                        :start pos1)
                     (length string)))
      (dolist (word *compound-separators*)
        (when (string-equal string word :start1 pos1 :end1 pos2)
          (let* ((string1 (substring string 0 pos0))
                 (string2 (string-pluralize-1a
                           (substring string pos0)
                           (- pos2 pos0)))
                 (length2 (length string2)))
            (return-from string-pluralize-1
              (string-pluralize-2
               string1
               #'(lambda (flush add length last-char-lc-flag)
                   (funcall continuation
                            (if flush
                                (+ flush length2)
                                length2)
                            (if add
                                (string-append add string2)
                                string2)
                            (+ length length2)
                            last-char-lc-flag)))))))
      (setq pos0 pos2)
      (setq pos1 (string-search-not-set *whitespace* string
                        :start pos2)))))

(defun string-pluralize-1a (string pos)
  (let* ((pos1 (or (string-search-not-set *whitespace* string
                                          :start pos)
                   pos))
         (pos2 (or (string-search-set *whitespace* string
                                      :start pos1)
                   (length string))))
    (cond ((or (string-equal string "a" :start1 pos1 :end1 pos2)
               (string-equal string "an" :start1 pos1 :end1 pos2))
           (string-append (substring string 0 pos1)
                          (string-pluralize
                           (substring
                            string
                            (or (string-search-not-set
                                 *whitespace*
                                 string
                                 :start pos2)
                                (length string))))))
          (t
           string))))

(defun string-pluralize-2 (string continuation)
  (let ((length (length string))
        (pos0 (1+ (or (string-search-set *whitespace* string
                                         :from-end t)
                      -1))))
    (if (zerop length)
        ""
        (let* ((flush nil)
               (add nil)
               (last-char-raw (aref string (1- length)))
               (last-char (char-upcase last-char-raw))
               (last-char-lc-flag (char/= last-char last-char-raw))
               (penult-char (if (> length 1)
                                (char-upcase (aref string (- length 2)))
                                #\*))
               (last-3 (substring string (max 0 (- length 3)))))
          (cond ((and (char-equal last-char #\Y)
                      (not (member penult-char '(#\A #\E #\I #\O #\U))))
                 (setq flush 1
                       add "ies"))
                ((or (string-equal string "ox" :start1 pos0)
                     (string-equal string "vax" :start1 pos0))
                 (setq add "en"))
                ((or (and (char= last-char #\H)
                          (member penult-char '(#\C #\S)))
                     (member last-char '(#\S #\Z #\X)))
                 (setq add "es"))
                ((and (string-equal last-3 "man")
                      (not (string-equal string "human" :start1 pos0)))
                 (setq flush 2
                       add "en"))
                ((string-equal last-3 "ife")
                 (setq flush 2
                       add "ves"))
                ((and (>= length 5)
                      (string-equal string "child" :start1 (- length 5)))
                 (setq add "ren"))
                (t
                 (setq add "s")))
          (funcall continuation flush add length last-char-lc-flag)))))

;;; SCL:STRING-PLURALIZE
(defun string-pluralize (string)
  (coerce-string-arg string)
  (string-pluralize-1
   string
   #'(lambda (flush add length last-char-lc-flag)
       (if flush
           (setq string (substring string 0 (- length flush))))
       (cond (add
              (let ((text-to-add (cond (last-char-lc-flag
                                        add)
                                       (t
                                        (string-upcase add)))))
                (string-append string text-to-add)))
             (t
              string)))))

;;; CLI::STRING-PLURALIZE-TO-STREAM
(defun string-pluralize-to-stream (string stream)
  (coerce-string-arg string)
  (string-pluralize-1
   string
   #'(lambda (flush add length last-char-lc-flag)
       (write-string stream string :end (if flush
                                            (- length flush)))
       (map nil #'(lambda (c)
                    (let ((char (if last-char-lc-flag
                                    c
                                    (char-upcase c))))
                      (write-char char stream)))
            add))))


;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ String articles:

(defvar *pronounce-aitch* t)

(defvar *string-a-or-an-exceptions-alist*
  '(("one" . :A)
    ("honor" . :AN)
    ("honour" . :AN))
  "List with form ((EXCEPTION1 . VAL1) ...) where VALi may be :A or
:AN.")

;;; SCL:STRING-A-OR-AN
(defun string-a-or-an (string &optional (both-words t) (case :downcase))
  "Computes whether the article ``a'' or ``an'' is used when introducing
a noun.  If BOTH-WORDS is true, the result is the concatenation of the
article, a space, and the noun; otherwise, the article is returned.  The
CASE argument controls the case of the article."
  (coerce-string-arg string)
  (let* ((length (length string))
         (pos (string-search #\Space string))
         (original-string string))
    (when pos
      (setf string (nsubstring string 0 pos)
            length pos))
    (if (= length 0)
        string
        (let ((char (char string 0)))
          (let* ((article
                  (cond
                    ((cdr (assoc string
                                 *string-a-or-an-exceptions-alist*
                                 :test #'string-equal)))
                    ((digit-char-p char)
                     (string-a-or-an
                      (format nil "~:R"
                              (parse-integer string :junk-allowed t))
                      nil
                      nil))
                    ((or
                      ;; "an x..." but "a xylophone"
                      (= length 1)
                      ;; "an fff" but "a frog"
                      (not (string-search-set "AEIOU" string))
                      ;; "An XL400" but "a xylophone"
                      (string-search-set "0123456789" string))
                     (if (string-search-char char "AEFHILMNORSX")
                         :an
                         :a))
                    (t
                     (if (or
                          ;; "an apple", "an indian", ...
                          (string-search-char char "AIO")
                          ;; "a history" (American) but "an history"
                          ;; (British)
                          (if (or (char= char #\H)
                                  (char= char #\h))
                              (ccase *pronounce-aitch*
                                ((t)
                                 nil)
                                ((:an)
                                 nil)
                                ((nil)
                                 (not (string-equal string "HEU"
                                                    :end1 3)))))
                          ;; "an egg" but "a eunich"
                          (and (or (char= char #\E)
                                   (char= char #\e))
                               (not (string-equal string "EU"
                                                  :end1 2)))
                          ;; "an umbrella", but "a unicorn",
                          ;; "uniform", but "an uninformed...", and of
                          ;; course " a unix"
                          (and (or (char= char #\U)
                                   (char= char #\u))
                               (not
                                (and
                                 (string-equal string "UNI"
                                               :end1 3)
                                 (or (< length 5)
                                     (not (string-search-char
                                           (char string 4)
                                           ;; Treat "y" as a
                                           ;; vowel
                                           "BCDFGHJKLMNPQRSTVWXZ")))))))
                         :an
                         :a))))
                 (article-string
                  (cond ((not (eq case 't))
                         (ecase case
                           ((nil)
                            article)
                           ((:downcase)
                            (string-downcase article))
                           ((:upcase)
                            (string-upcase article))
                           ((:capitalize)
                            (string-capitalize article))))
                        ((upper-case-p (char string (- length 1)))
                         (string-upcase article))
                        (t
                         (string-downcase article)))))
            (if both-words
                (concatenate 'string article-string " " original-string)
                article-string))))))

;;; }}}
;;; ------------------------------------------------------------------

;;; }}}
;;; ==================================================================

;;; symbolics.lisp ends here
