;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; for.lisp --- Trivial iteration protocol
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:43:08 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2004 Tim Bradshaw
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 17:00:36
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
;;; Rather trivial iteration protocol
;;; $Id: //depot/www-tfeb-org/main/www-tfeb-org/html/programs/lisp/for.lisp#1 $
;;;
;;; Copyright 2004 by me, Tim Bradshaw, and may be used for any
;;; purpose whatsoever by anyone. It has no warranty whatsoever. I
;;; would appreciate acknowledgement if you use it in anger, and I
;;; would also very much appreciate any feedback or bug fixes.
;;; -- tfb+lisp-toys@tfeb.org
;;;
;;;}}}

#-genera
(in-package #:cl-hacks)

;;; ------------------------------------------------------------------
;;;{{{ Iteration protocol:

(defgeneric iter (x &key)
  (:documentation "Return an iterator for X")
  (:method (x &key)
   (error "No iterator defined for class ~S"
          (class-name (class-of x)))))

(defgeneric next (iter)
  (:documentation "Step an iterator.
   Returns either next and T or NIL and NIL when the iterator is exhausted")
  ;; simple iterators are functions: just call them
  (:method ((iter t))
   (funcall iter)))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Generators:

;;; bindings is either (v form &optional result) or 
;;; ((v1 form1 &optional r1) ...).  Constructs iterators for all the forms,
;;; steps them in parallel until the first is exhausted, then returns
;;; a value for each form.  All forms are stepped the same numnber of times.
;;; the variables are assigned to on each step unless the iterator
;;; is exhausted, when they keep their old values (this may or may not 
;;; be the right semantics).
(defmacro for (bindings &body body)
  (multiple-value-bind (vars forms result-forms)
      (etypecase (first bindings)
        (symbol
         (destructuring-bind (var form &optional result-form) bindings
           (values (list var) (list form) (list result-form))))
        (cons
         (loop with var and form and result-form
               for binding in bindings
               do (setf (values var form result-form) 
                        (destructuring-bind 
                              (var form &optional result-form)
                            binding
                          (values var form result-form)))
               collect var into vars 
               collect form into forms
               collect result-form into result-forms
               finally (return (values vars forms result-forms)))))
    (let ((iters
           (loop for i in vars
                 collect (make-symbol
                          (concatenate 'string
                                       "I-" (symbol-name i)))))
          (cont (make-symbol "CONT")))
      `(let ((,cont t)
             ,@(loop for i in iters
                     for f in forms
                     collect `(,i ,(if (and (consp f)
                                            (eql (car f) 'iter))
                                       ;; if it's already (ITER ...),
                                       ;; just leave it.
                                       f
                                       `(iter ,f)))))
         ;; This won't help, but it's true.
         (declare (dynamic-extent ,@iters))
         (do ,(loop for i in iters
                    for v in vars
                    collect `(,v (multiple-value-bind (.n. .cont.)
                                     (next ,i)
                                   (unless .cont.
                                     (setf ,cont nil))
                                   .n.)
                                 (multiple-value-bind (.n. .cont.)
                                     (next ,i)
                                   (if .cont.
                                       .n.
                                     (progn
                                       (setf ,cont nil)
                                       ,v)))))
             ((not ,cont) (values ,@result-forms))
           ,@body)))))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Useful iterators:

(defmethod iter ((l list) &key)
  #'(lambda ()
      (if l
          (multiple-value-prog1
              (values (car l) t)
            (setf l (cdr l)))
          (values nil nil))))

(defmethod iter ((v vector) &key)
  (let ((i 0)
        (l (length v)))
    #'(lambda ()
        (if (< i l)
            (multiple-value-prog1
                (values (aref v i) t)
              (incf i))
            (values nil nil)))))

(defmethod iter ((i integer) &key)
  (unless (> i 0)
    (error "Negative iteration range?"))
  (let ((n 0))
    #'(lambda ()
        (if (= i n)
            (values nil nil)
            (multiple-value-prog1
                (values n t)
              (incf n))))))

(defmethod iter ((f float) &key)
  ;; truncate?  Seems right
  (iter (truncate f)))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Ranges:

;;; Ranges are mutable, use-once things.  The alternative would be 
;;; to have them immutable, and have their iterators return an object
;;; which enumerates them,  That might be better.
(defstruct range
  (current 0)
  (limit 1)
  (step 1))

;;; make a range.  Process arguments by hand since although they're
;;; keywords, the order matters - :below 10 :to 4 :by 3 is different
;;; than :from 10 :below 20 :by 3.  To do a professional job of this
;;; it would probably be best to use a compiler-macro to rewrite this
;;; to an internal positional function, as this arg parsing is hairy 
;;; at runtime.
(defun range (&rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  ;; first we want a start spec
  (let (start step limit)
    (when (null args)
      (error "No range spec at all!"))
    (destructuring-bind (key val . rest) args
      (case key
        ((:from )
 (setf start val))
        ((:above)
 (setf start (1+ val)
       step 1))
        ((:below)
 (setf start (1- val)
       step -1))
        (otherwise
         (error "Bad start spec ~S" key)))
      ;; if there is no more then just make some open range,
      ;; otherwise look for finish or step
      (when rest
        (destructuring-bind (key val . rest) rest
          (case key
            ((:to)
     (setf limit val
   step (or step (truncate 
                                  (signum (- limit start))))))
            ((:above)
     (setf limit (1+ val)
   step (or step -1)))
            ((:below)
     (setf limit (1- val)
   step (or step 1)))
            ((:by :step)
     (setf step val))
            (otherwise
             (error "Bad finish/step spec ~S" key)))
          ;; now just try again (yes, duplicated code, ick)
          (when rest
            (destructuring-bind (key val . rest) rest
              (case key
                ((:to)
                 (setf limit val
                       step (or step (truncate 
                                      (signum (- limit start))))))
                ((:above)
                 (setf limit (1+ val)
                       step (or step -1)))
                ((:below)
                 (setf limit (1- val)
                       step (or step 1)))
                ((:by :step)
                 (setf step val))
                (otherwise
                 (error "Bad finish/step spec ~S" key)))
              (when rest
                (error "surplus arguments in range spec: ~S" rest)))))))
    (make-range :current start :step step :limit limit)))

(defmethod iter ((r range) &key)
  r)

(defmethod next ((r range))
  (let* ((s (range-step r))
         (l (range-limit r))
         (c (range-current r))
         (next (+ c s)))
    (if (>= s 0)
        ;; This deals with the = 0 case as well
        (if (and l (> c l))
            (values nil nil)
            (progn
              (setf (range-current r) next)
              (values c t)))
        (if (and l (< c l))
            (values nil nil)
            (progn
              (setf (range-current r) next)
              (values c t))))))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ List comprehension:

;;; Python-style list comprehension operator.  See examples below
;;; Each clause is simply rewritten to a `single-special-argument' macro:
;;; foo x ... -> (foo x ...), so you can put anything there, even your
;;; own macros: (gather x dolist (x '(1 2 3 4))) works.
(defmacro gather (form &body clauses)
  (let ((an (make-symbol "A"))
        (atn (make-symbol "AT")))
    `(let ((,an '())
           (,atn '()))
       ,(labels ((make-form (ct)
                   (cond ((null ct)
                          ;; done: collect form
                          `(if ,an
                               (setf (cdr ,atn) (list ,form)
                                     ,atn (cdr ,atn))
                               (setf ,an (list ,form)
                                     ,atn ,an)))
                         ((null (rest ct))
                          ;; Should be even length
                          (error "Malformed clause list: ~{~S~^ ~}"
                                 clauses))
                         (t
                          `(,(first ct)
                             ,(second ct)
                             ,(make-form (cddr ct)))))))
          (make-form clauses))
       ,an)))

#||
(gather (* x x)
  for (x '(1 2 t 3))
  when (numberp x)
  when (oddp x))

(gather x
  for (l '((1 nil 2) (3 4 t)))
  for (x l)
  when (numberp x))

(gather x
  for (c (range :from 0 :below 100))
  let ((x (random 100)))
  when (evenp x))
||#

;;;}}}
;;; ------------------------------------------------------------------

;;; for.lisp ends here
