;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; lists.lisp --- List functions
;;;
;;; Time-stamp: <Friday Dec  9, 2011 10:03:07 asmodai>
;;; Revision:   5
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Dec 2011 01:21:05
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
(in-package #:cl-hacks-internals)

(defun alist-plist (alist)
  (let (plist)
    (dolist (pair alist)
      (push (car pair) plist)
      (push (cdr pair) plist))
    (nreverse plist)))

(defun plist-alist (plist)
  (let (alist)
    (do ((tail plist (cddr tail)))
        ((endp tail) (nreverse alist))
      (push (cons (car tail) (cadr tail)) alist))))

(defun malformed-plist (plist)
  (error "Malformed plist: ~S" plist))

(defmacro doplist ((key val plist &optional values) &body body)
  (multiple-value-bind (forms declarations) (parse-body body)
    (with-gensyms (tail loop results)
      `(block nil
         (flet ((,results ()
                  (let (,key ,val)
                    (declare (ignorable ,key ,val))
                    (return ,values))))
           (let* ((,tail ,plist)
                  (,key (if ,tail
                            (pop ,tail)
                            (,results)))
                  (,val (if ,tail
                            (pop ,tail)
                            (malformed-plist ',plist))))
             (declare (ignorable ,key ,val))
             ,@declarations
             (tagbody
               ,loop
               ,@forms
               (setf ,key (if ,tail
                              (pop ,tail)
                              (,results))
                     ,val (if ,tail
                              (pop ,tail)
                              (malformed-plist ',plist)))
               (go ,loop))))))))

(define-modify-macro appendf (&rest lists) append
  "Modify-macro for APPEND. Appends LISTS to the place designated by
the first argument.")

(define-modify-macro nconcf (&rest lists) nconc
  "Modify-macro for NCONC. Concatenates LISTS to place designated by
the first argument.")

(define-modify-macro unionf (list) union
  "Modify-macro for UNION. Saves the union of LIST and the contents of
the place designated by the first argument to the designated
place.")

(define-modify-macro nunionf (list) nunion
  "Modify-macro for NUNION. Saves the union of LIST and the contents
of the place designated by the first argument to the designated
place. May modify either argument.")

(defun circular-list (&rest elements)
  (let ((cycle (copy-list elements)))
    (nconc cycle cycle)))

(defun list-to-circular-list (list)
  (setf (cdr (last list)) list))

(defun circular-list-p (object)
  (and (listp object)
       (do ((fast object (cddr fast))
            (slow (cons (car object) (cdr object)) (cdr slow)))
           (nil)
         (unless (and (consp fast) (listp (cdr fast)))
           (return nil))
         (when (eq fast slow)
           (return t)))))

(defun circular-tree-p (object)
  (labels ((circularp (object seen)
             (and (consp object)
                  (do ((fast (cons (car object) (cdr object))
                             (cddr fast))
                       (slow object (cdr slow)))
                      ((or (not (consp fast))
                           (not (consp (cdr slow))))
                       (do ((tail object (cdr tail)))
                           ((not (consp tail))
                            nil)
                         (let ((elt (car tail)))
                           (circularp elt (cons object seen)))))
                    (when (or (eq fast slow) (member slow seen))
                      (return-from circular-tree-p t))))))
    (circularp object nil)))

(defun proper-list-p (object)
  (cond ((not object)
         t)
        ((consp object)
         (do ((fast object (cddr fast))
              (slow (cons (car object) (cdr object)) (cdr slow)))
             (nil)
           (unless (and (listp fast)
                        (consp (cdr fast)))
             (return (and (listp fast)
                          (not (cdr fast)))))
           (when (eq fast slow)
             (return nil))))
        (t
         nil)))

(deftype proper-list ()
  `(and list (satisfies proper-list-p)))

(defun lastcar (list)
  (do ((last list fast)
       (fast list (cddr fast))
       (slow (cons (car list) (cdr list)) (cdr slow)))
      (nil)
    (when (endp fast)
      (return (cadr last)))
    (when (endp (cdr fast))
      (return (car fast)))
    (when (eq fast slow)
      (error 'type-error
             :datum list
             :expected-type '(and list (not circular-list))))))

(defun (setf lastcar) (object list)
  (do ((last list fast)
       (fast list (cddr fast))
       (slow (cons (car list) (cdr list)) (cdr slow)))
      (nil)
    (when (endp fast)
      (return (setf (cadr last) object)))
    (when (endp (cdr fast))
      (return (setf (car fast) object)))
    (when (eq fast slow)
      (error 'type-error
             :datum list
             :expected-type '(and list (not circular-list))))))

(defun make-circular-list (length &key initial-element)
  (let ((cycle (make-list length :initial-element initial-element)))
    (nconc cycle cycle)))

(deftype circular-list ()
  `(satisfies circular-list-p))

(defun ensure-car (thing)
  (if (consp thing)
      (car thing)
      thing))

(defun ensure-cons (cons)
  (if (consp cons)
      cons
      (cons cons nil)))

(defun ensure-list (list)
  (if (listp list)
      list
      (list list)))

(defun remove-from-plist (plist &rest keys)
  (declare (optimize (speed 3)))
  (loop for (key . rest) on plist by #'cddr
        do (assert rest () "Expected a proper plist, got ~S" plist)
        unless (member key keys :test #'eq)
          collect key and collect (first rest)))

(defun delete-from-plist (plist &rest keys)
  ;; FIXME: should not cons
  (apply 'remove-from-plist plist keys))

(define-modify-macro remove-from-plistf (&rest keys)
  remove-from-plist)

(define-modify-macro delete-from-plistf (&rest keys)
  delete-from-plist)

(defsubst sans (plist &rest keys)
  (apply #'remove-from-plist plist keys))

(defun mappend (function &rest lists)
  (loop for results in (apply #'mapcar function lists)
        append results))

(defun setp (object &key (test #'eql) (key #'identity))
  (and (listp object)
       (let (seen)
         (dolist (elt object t)
           (let ((key (funcall key elt)))
             (if (member key seen :test test)
                 (return nil)
                 (push key seen)))))))

(defun set-equal (list1 list2 &key (test #'eql) (key nil keyp))
  (let ((keylist1 (if keyp (mapcar key list1) list1))
        (keylist2 (if keyp (mapcar key list2) list2)))
    (and (dolist (elt keylist1 t)
           (or (member elt keylist2 :test test)
               (return nil)))
         (dolist (elt keylist2 t)
           (or (member elt keylist1 :test test)
               (return nil))))))

(defun map-product (function list &rest more-lists)
  (labels ((%map-product (f lists)
             (let ((more (cdr lists))
                   (one (car lists)))
               (if (not more)
                   (mapcar f one)
                   (mappend (lambda (x)
                              (%map-product (curry f x) more))
                            one)))))
    (%map-product (if (functionp function)
                      function
                      (fdefinition function))
                  (cons list more-lists))))

(defun flatten (tree)
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))

#-lispworks
(defmacro push-end (item item-list)
  `(setf ,item-list (nconc ,item-list (cons ,item nil))))

;;; lists.lisp ends here
