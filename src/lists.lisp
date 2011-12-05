;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; lists.lisp --- Various list functionality
;;;
;;; Time-stamp: <Monday Dec  5, 2011 05:07:14 asmodai>
;;; Revision:   7
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Dec 2011 16:08:26
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
(in-package #:cl-hacks)

#||
Return the last two elements in a list.
||#
(defun last-but-one (list)
  (cond ((endp list)
         nil)
        ((endp (rest list))
         nil)
        (t
         (last list 2))))

#||
This is like NTH, but the index base is 1 rather than 0.
||#
(defun element-at (list pos)
  (cond ((not (integerp pos))
         (error "Non integer index ~A" pos))
        ((not (plusp pos))
         (error "Non positive index ~A" pos))
        (t
         (elt list (1- pos)))))

#||
Returns T if the list is palindromic.
||#
(defun list-palindromic-p (list)
  (loop with data = (coerce list 'vector)
        for i from 0
        for j from (1- (length data)) by -1
        while (< i j)
        always (eql (aref data i) (aref data j))))

#||
Elimininates consecutive duplicates of the list given in LIST.

If the list contains repeated elements, they are replaced with a
single copy of the element.  The order of the elements is not
changed.
||#
(defun compress-list (list)
  (reduce #'(lambda (item result)
              (cond ((endp result)
                     (list item))
                    ((eql (first result) item)
                     result)
                    (t
                     (cons item result))))
          list
          :from-end t
          :initial-value '()))

#||
Pack consecutive duplicates of list elements into a sublist.
||#
(defun pack-list (list)
  (reduce #'(lambda (item result)
              (cond ((endp result)
                     (list (list item)))
                    ((eql (first (first result)) item)
                     (cons (cons item (first result))
                           (rest result)))
                    (t
                     (cons (list item) result))))
          list
          :from-end t
          :initial-value '()))
            

(defun run-length-encode (list)
  (when list
    (loop with count = 0
          with last-item = nil
          with result = '()
          for item in list
          do (cond ((zerop count)
                    (setf count 1
                          last-item item))
                   ((eql item last-item)
                    (incf count))
                   (t
                    (push (list count last-item) result)
                    (setf count 1
                          last-item item)))
             finally (when (plusp count)
                       (push (list count last-item) result))
                     (return-from run-length-encode
                       (nreverse result)))))

#||
Runlength-encode a list.
||#
(defun run-length-encode/simplified (list)
  (let ((result '())
        (count 0)
        (last-item nil))
    (labels ((collect-result ()
               (push (if (= 1 count)
                         last-item
                         (list count last-item))
                     result))
             (new-item (item)
               (setf count 1
                     last-item item))
             (same-item ()
               (incf count))
             (return-result ()
               (when (plusp count)
                 (collect-result))
               (nreverse result)))
      (dolist (item list (return-result))
        (cond ((zerop count)
               (new-item item))
              ((eql item last-item)
               (same-item))
              (t
               (collect-result)
               (new-item item)))))))

#||
Decode a runlength-encoded list.
||#
(defun run-length-decode (rle-list)
  (loop with result = '()
        for item in (reverse rle-list)
        do (if (atom item)
               (push item result)
               (loop repeat (first item)
                     do (push (second item) result)))
        finally (return-from run-length-decode result)))

#||
Replicate the elements of a list a given number of times.
||#
(defun replicate-list (list count)
  (mapcan #'(lambda (item)
              (make-list count :initial-element item))
          list))

#||
Duplicate the elements of a list.
||#
(defsubst duplicate-list (list)
  (replicate-list list 2))

#||
Drop every NUM element from LST.
||#
(defun drop-every-nth (list elm &aux (m (1- elm)))
  (loop for a = list then (cdr b)
        for b = (nthcdr m a)
        if b
          nconc (ldiff a b)
        else
          nconc a
          and
            do (loop-finish)))

#||
Split a list into two parts.
||#
(defun split-list (list count)
  (list (subseq list 0 count)
        (nthcdr count list)))


#||
Create a slice of the list given in LST.  The slice starts at START
and ends at END.
||#
(defun slice-list (lst start end)
  (subseq lst (1- start) end))

#||
Rotate a the list LST by PLACES.
||#
(defun rotate-list (lst places)
  (if (minusp places)
      (rotate lst (+ (length lst) places))
      (nconc (subseq lst places)
             (subseq lst 0 places))))

#||
Remove the element of the list LST at position POS.
||#
(defun remove-at (lst pos)
  (remove-if (constantly t) lst
             :start (1- pos)
             :end pos))

#||
Insert WHAT as a new element of the list LST at the position POS.
||#
(defun insert-at (what lst pos)
  (append (subseq lst 0 (1- pos))
          (list what)
          (nthcdr (1- pos) lst)))

#||
Create a list containing all integers within a given range.
||#
(defun numeric-range (start last)
  (loop for i from start to last
        collect i))

#||
Extract a given number of randomly selected elements from a list.
||#
(defun random-select (list count)
  (let ((len (length list)))
    (cond ((zerop count)
           '())
          ((<= 1 count len)
           (loop with indices = '()
                 with result = '()
                 while (plusp count)
                 for i = (random len)
                 unless (member i indices)
                   do (progn
                        (push i indices)
                        (push (elt list i) result)
                        (decf count))
                 finally (return-from random-select result)))
          (t
           (error "Invalid count, must be between 0 and ~D" len)))))

#||
Draw N different random numbers from the set 1..M.
||#
(defun lottery-select (selection set-size)
  (random-select (range 1 set-size) selection))

#||
Generate a random permutation of the elements of a list.
||#
(defun random-permutation (list)
  (when list
    (loop with len = (length list)
          with choices = (list-to-circular-list (copy-list list))
          collect (pop (cdr (nthcdr (random len) choices)))
          while (plusp (decf len)))))

#||
Generate the combinations of K distinct objects chosen from the N
elements of a list.
||#
(defun list-combinations (count list)
  (cond ((zerop count)
         '(()))
        ((endp list)
         '())
        (t
         (nconc (mapcar #'(lambda (combi)
                            (cons (first list) combi))
                        (list-combinations (1- count) (rest list)))
                (list-combinations count (rest list))))))

#||
Group the elements of a set into disjoint subsets.
||#
(defun list-group (set sizes)
  (cond ((endp sizes)
         (error "Not enough ``sizes'' given."))
        ((endp (rest sizes))
         (if (= (first sizes) (length set))
             (list (list set))
             (error "Cardinal mismatch: |set| = ~A; required ~A"
                    (length set)
                    (first sizes))))
        (t
         (mapcan #'(lambda (combi)
                     (mapcar #'(lambda (group)
                                 (cons combi group))
                             (list-group (set-difference set combi)
                                         (rest sizes))))
                 (list-combinations (first sizes) set)))))

#||
Group the elements of a set into subsets of 2, 3 and 4 elements.
||#
(defun list-group3 (set)
  (list-group set '(2 3 4)))

#||
Sort a list of lists according to the length of sublists.
||#
(defun lsort (lists)
  (map 'list (function cdr)
       (sort (map 'vector #'(lambda (list)
                              (cons (length list) list))
                  lists)
             (function <)
             :key (function car))))

#||
Sort a list of lists according to the length frequency.
||#
(defun lfsort (lists)
  (let* ((data (map 'vector #'(lambda (list)
                                (cons (length list) list))
                    lists))
         (histo (make-histogram data :key (function car))))
    (map 'list (function cdr)
         (sort data
               (function <)
               :key #'(lambda (item)
                        (gethash (car item) histo))))))

#||
Make OBJECT into a list if OBJECT is an atom.
||#
(defun mklist (object)
  (if (listp object)
      object
      (list object)))

#||
Map a list by function and eliminate elements where FN returns NIL.
||#
(defun map-and-remove-nils (fn list)
  (let ((acc nil))
    (dolist (x list (nreverse acc))
      (let ((val (funcall fn x)))
        (when val
          (push val acc))))))

#||
Filter a list by function and eliminate elements where FN returns NIL.
||#
(defun filter-list (fn list)
  (let ((acc nil))
    (dolist (x list (nreverse acc))
      (when (funcall fn x)
        (push x acc)))))

#||
Append two lists, filtering out elements from the second list that are
already in the first list.
||#
(defun append-list (l1 l2)
  (dolist (elem l2 l1)
    (unless (find elem l1)
      (setq l1 (append l1 (list elem))))))

#||
Strip from a tree of atoms that satisfy a given predicate.
||#
(defun remove-from-tree-if (pred tree &optional atom-processor)
  (if (atom tree)
      (unless (funcall pred tree)
        (if atom-processor
            (funcall atom-processor tree)
            tree))
      (let ((car-strip (remove-from-tree-if
                        pred
                        (car tree)
                        atom-processor))
            (cdr-strip (remove-from-tree-if
                        pred
                        (cdr tree)
                        atom-processor)))
        (cond
          ((and car-strip (atom (cadr tree)) (null cdr-strip))
           (list car-strip))
          ((and car-strip cdr-strip)
           (cons car-strip cdr-strip))
          (car-strip
           car-strip)
          (cdr-strip
           cdr-strip)))))

#||
Find an atom as a car in a tree and return the cdr of the tree at that
position.
||#
(defun find-in-tree (sym tree)
  (if (or (null tree)
          (atom tree))
      nil
      (if (eql sym (car tree))
          (cdr tree)
          (aif (find-in-tree sym (car tree))
               it
               (aif (find-in-tree sym (cdr tree))
                    it
                    nil)))))

#||
Remove the given keyword from an argument list.
||#
;;; This symbol is somehow in USER on my Genera world, but it isn't
;;; defined.
(defun remove-keyword (key arglist)
  (loop for sublist = arglist then rest until (null sublist)
        for (elt arg . rest) = sublist
        unless (eq key elt)
          append (list elt arg)))

#||
Remove all the given keywords from an argument list.
||#
(defun remove-keywords (keys arglist)
  (loop for (name val) on arglist by #'cddr
        unless (member (symbol-name name) keys
                       :key #'symbol-name
                       :test 'equal)
          append (list name val)))

#||
Concatenate the results of MAPCAR lambda calls.  This function does
not use tail recursion.
||#
(defun mapcar-append-string-nontailrec (func v)
  (aif (car v)
       (concatenate 'string (funcall func it)
                    (mapcar-append-string-nontailrec func (cdr v)))
       ""))

#||
Concatenate the results of MAPCAR lambda calls.
||#
(defun mapcar-append-string (func v &optional (accum ""))
  (aif (car v)
       (mapcar-append-string
        func
        (cdr v)
        (concatenate 'string accum (funcall func it)))
       accum))

#||
Concatenate the results of MAPCAR lambda calls over two lists.  This
function does not use tail recursion.
||#
(defun mapcar/2-append-string-nontailrec (func l1 l2)
  (let ((a (car l1))
        (b (car l2)))
    (if (and a b)
        (concatenate
         'string 
         (funcall func a b)
         (mapcar/2-append-string-nontailrec func (cdr l1) (cdr l2)))
        "")))

#||
Concatenate the results of MAPCAR lambda calls over two lists.
||#
(defun mapcar/2-append-string (func l1 l2 &optional (accum ""))
  (let ((a (car l1))
        (b (car l2)))
    (if (and a b)
        (mapcar/2-append-string
         func
         (cdr l1)
         (cdr l2)
         (concatenate 'string accum (funcall func a b)))
        accum)))

#||
Take a list of lists and append all sublists.
||#
(defun append-sublists (list)
  (let ((results (car list)))
    (dolist (elem (cdr list) results)
      (setq results (append results elem)))))

;;;
;;; The alist stuff below could possible be better placed in CLHI.
;;;

#||
Returns T if the object is an alist element.
||#
(defun alist-elem-p (elem)
  (and (consp elem)
       (atom (car elem))
       (atom (cdr elem))))

#||
Returns T if the object is an alist.
||#
(defun alistp (alist)
  (when (listp alist)
    (dolist (elem alist)
      (unless (alist-elem-p elem)
        (return-from alistp nil)))
    t))

(defmacro %update-alist (akey value alist
                         &key (test '#'eql) (key '#'identity))
  (let ((elem (gensym "ELEM-"))
        (val (gensym "VAL-")))
    `(let ((,elem (assoc ,akey ,alist :test ,test :key ,key))
           (,val ,value))
       (cond (,elem
              (set (cdr ,elem) ,val))
             (,alist
              (setf (cdr (last ,alist)) (list (cons ,akey ,val))))
             (t
              (setf ,alist (list (cons ,akey ,val)))))
       ,alist)))

#||
Get an element of an alist.
||#
(defun get-alist (key alist &key (test #'eql))
  (cdr (assoc key alist :test test)))

#||
Set the element of an alist.
||#
(defun (setf get-alist) (value key alist &key (test #'eql))
  (%update-alist key value alist :test test)
  value)

#||
Obtain a unique list of slot values from a list of instances.
||#
(defun unique-slot-values (list slot &key (test 'eql))
  (let ((uniq '()))
    (dolist (item list (nreverse uniq))
      (let ((value (slot-value item slot)))
        (unless (find value uniq :test test)
          (push value uniq))))))

;;; lists.lisp ends here
