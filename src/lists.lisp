;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; lists.lisp --- List-based functions
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:29:51 asmodai>
;;; Revision:   13
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2002 Keven M. Rosenberg
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

(defun last-but-one (lst)
  "Return the last two elements in a list."
  (let ((rlst (reverse lst)))
    (cond ((null rlst)
	   nil)
	  ((<= (length rlst) 2) 
	   lst)
	  (t
	   (list (second rlst) (first rlst))))))

;; This is like nth, but the index base is 1
(defun element-at (lst pos &optional (initial 1))
  (if (eql initial pos)
      (car lst)
      (element-at (cdr lst) pos (1+ initial))))

(defun flatten-list (lst)
  (cond ((atom lst)
	 lst)
	((listp (car lst))
	 (append (flatten-list (car lst)) (flatten-list (cdr lst))))
	(t
	 (append (list (car lst)) (flatten-list (cdr lst))))))
;;
;;
(defun list-palindromic-p (lst)
  (equal lst (reverse lst)))

(defun compress-list (lst)
  (cond ((null lst)
	 lst)
	((null (cdr lst))
	 lst)
	((eql (car lst) (cadr lst))
	 (compress-list (cdr lst)))
	(t
	 (cons (car lst) (compress-list (cdr lst))))))


(defun pack-list-group (lst)
  (cond ((eql lst nil)
	 nil)
	((eql (cdr lst) nil)
	 lst)
	((equal (car lst) (cadr lst))
	 (cons (car lst) (pack-list-group (cdr lst))))
	(t
	 (list (car lst)))))

(defun pack-list-rest (lst)
  (cond ((eql lst nil)
	 nil)
	((eql (car lst) (cadr lst))
	 (pack-list-rest (cdr lst)))
	(t
	 (cdr lst))))

(defun pack-list (lst)
  (if (eql lst nil)
      nil
      (cons (pack-list-group lst) (pack-list (pack-list-rest lst)))))

(defun run-length-encode (lst)
  (labels ((rlencode (lst res &aux (c 0))
	     (if lst
		 (rlencode (member-if-not #'(lambda (e)
					      (if (eql (car lst) e)
						  (incf c)))
					  lst)
			   (cons (if (= c 1)
				     (car lst)
				     (list c (car lst)))
				 res))
		 (nreverse res))))
    (rlencode lst nil)))

(defun unpack-rle (lst)
  (loop for i from 1 to (car lst)
	collect (cadr lst)))

(defun run-length-decode (lst)
  (cond ((null lst)
	 nil)
	((atom (car lst))
	 (append (list (car lst)) (run-length-decode (cdr lst))))
	((listp (car lst))
	 (append (unpack-rle (car lst)) (run-length-decode (cdr lst))))))

(defun duplicate-list (lst)
  (if (eql lst nil)
      nil
      (append (list (car lst) (car lst)) (duplicate-list (cdr lst)))))

(defun replicate-elem (lst count)
  (labels ((replicate-aux (e c)
	     (loop for i from 1 to c
		   collect e)))
    (if (eql lst nil)
	nil
	(append (replicate-aux (car lst) count)
		(replicate-elem(cdr lst) count)))))

(defun drop-every-nth (lst num)
  (loop for c from 0 to (1- (length lst))
	when (> (mod (1+ c) num) 0)
	  collect (nth c lst)))

(defun split-list-before (lst pos &optional (initial 0))
  (if (> pos initial)
      (cons (car lst) (split-list-before (cdr lst) pos (1+ initial)))))

(defun split-list-after (lst pos &optional (initial 0))
  (if (> pos initial)
      (split-list-after (cdr lst) pos (1+ initial))
      lst))

(defun split-list (lst pos)
  (if (and pos (listp lst))
      (list (split-list-before lst pos) (split-list-after lst pos))))
  
(defun slice-list (lst start end)
  (if (null lst)
      nil
      (loop for idx from 0 to (1- (length lst))
	    when (and (>= (1+ idx) start)
		      (<= (1+ idx) end))
	      collect (nth idx lst))))

(defun rotate-list (lst places)
  (if (null lst)
      nil
      (if (< places 0)
	  (append (split-list-after lst (+ (length lst) places))
		  (split-list-before lst (+ (length lst) places)))
	  (append (split-list-after lst places)
		  (split-list-before lst places)))))

(defun remove-at (lst pos)
  (append (split-list-before lst (1- pos))
	  (split-list-after lst pos)))

(defun insert-at (what lst pos)
  (append (split-list-before lst (1- pos))
	  (list what)
	  (split-list-after lst (1- pos))))

;;;
;;; move to math.lisp
(defun make-integer-range (start stop)
  (loop for i from start to stop
	collect i))
;;;
;;;

(defun select-random (lst pos &optional (selected 0))
  (if (eql pos selected)
      nil
      (let ((rand-pos (1+ (random (length lst)))))
	(cons (element-at lst rand-pos)
	      (select-random (remove-at lst rand-pos) pos (1+ selected))))))

(defun random-permutation (lst)
  (select-random lst (length lst)))

(defun combination-list-elem (elem-list lst)
  (if (eql lst nil)
      nil
      (append (list (append elem-list (list (car lst))))
	      (combination-list-elem elem-list (cdr lst)))))

(defun combination (n-elem lst)
  (let ((num-elem (1- n-elem)))
    (if (or (> num-elem (length lst))
	    (< n-elem 2))
	nil
	(let ((elem-list (split-list-before lst num-elem))
	      (rest-list (split-list-after lst num-elem)))
	  (append (combination-list-elem elem-list rest-list)
		  (combination n-elem (cdr lst)))))))

(defun remove-list (lst elem)
  (if (eql elem nil)
      lst
      (remove-list (remove (car elem) lst) (cdr elem))))

(defun group-n (num-elem res lst)
  (combination-list-elem res (combination num-elem
					  (remove-list lst
						       (flatten-list res)))))

(defun group-n-list (num-elem res lst)
  (if (eql res nil)
      nil
      (let ((first-elem (car res))
	    (rest-list (cdr res)))
	(append (group-n num-elem first-elem lst)
		(group-n-list num-elem rest-list lst)))))

(defun group3 (lst)
  (group-n-list 5 (group-n-list 2 (group-n-list 2 nil lst) lst) lst))

(defun group-elements (lst elem-list)
  (if (eql (cdr elem-list) nil)
      (group-n (car elem-list) nil lst)
      (let ((elem-rlist (reverse elem-list)))
	(group-n-list (car elem-rlist)
		      (group-elements lst (reverse (cdr elem-rlist))) lst))))

(defun lsort (lst)
  (sort lst #'< :key #'length))

(defun lfreq (lst e-length)
  (if (eql lst nil)
      0
      (if (eql (length (car lst)) e-length)
	  (1+ (lfreq (cdr lst) e-length))
	  (lfreq (cdr lst) e-length))))

(defun lfmark (rest &optional (lst rest))
  (if (eql lst nil)
      nil
      (cons (list (lfreq lst (length (car rest))) (car rest))
	    (lfmark (cdr rest) lst))))

(defun lfunmark (lst)
  (if (eql lst nil)
      nil
      (cons (second (car lst)) (lfunmark (cdr lst)))))

(defun lfsort (lst)
  (let ((marked-list (lfmark lst)))
    (lfunmark (sort marked-list #'< :key #'(lambda (x) (car x))))))

;;
;; The following have been borrowed from the KMRCL package
;;

;; This isn't called MAKE-LIST because we don't want to clobber something defined elsewhere
(defun mklist (obj)
  "Make OBJ into a list if OBJ is an atom."
  (if (listp obj)
      obj
      (list obj)))

(defun map-and-remove-nils (fn lst)
  "Map a list by function, eliminate elements where FN returns NIL."
  (let ((acc nil))
    (dolist (x lst (nreverse acc))
      (let ((val (funcall fn x)))
	(when val
	  (push val acc))))))

(defun filter-list (fn lst)
  "Filter a list by function, eliminating elements where FN returns NIL."
  (let ((acc nil))
    (dolist (x lst (nreverse acc))
      (when (funcall fn x)
	(push x acc)))))

(defun append-list (l1 l2)
  "Append two lists, filtering out elems from the second list that are already in the first."
  (dolist (elem l2 l1)
    (unless (find elem l1)
      (setq l1 (append l1 (list elem))))))

(defun remove-from-tree-if (pred tree &optional atom-processor)
  "Strip from a tree of atoms that satisfy a given predicate."
  (if (atom tree)
      (unless (funcall pred tree)
	(if atom-processor
	    (funcall atom-processor tree)
	    tree))
      (let ((car-strip (remove-from-tree-if pred (car tree) atom-processor))
	    (cdr-strip (remove-from-tree-if pred (cdr tree) atom-processor)))
	(cond
	  ((and car-strip (atom (cadr tree)) (null cdr-strip))
	   (list car-strip))
	  ((and car-strip cdr-strip)
	   (cons car-strip cdr-strip))
	  (car-strip
	   car-strip)
	  (cdr-strip
	   cdr-strip)))))

(defun find-in-tree (sym tree)
  "Find an atom as a car in a tree and returns the cdr of the tree at that position."
  (if (or (null tree) (atom tree))
      nil
      (if (eql sym (car tree))
	  (cdr tree)
	  (aif (find-in-tree sym (car tree))
	       it
	       (aif (find-in-tree sym (cdr tree))
		    it
		    nil)))))

#-(or ecl genera)
(defun remove-keyword (key arglist)
  (loop for sublist = arglist then rest until (null sublist)
	for (elt arg . rest) = sublist
	unless (eq key elt) append (list elt arg)))

;;; Stop ecl from bitching
#+ecl
(defun remove-keyword (key arglist)
  (declare (ignore key arglist)))

#+genera
(defun remove-keyword (key arglist)
  "Remove the given keyword from a list."
  (zl:loop for elem in arglist
     if (listp elem)
       collect (remove-keyword key elem)
     else
       unless (eq key elem)
	 collect elem))

(defun remove-keywords (key-names arglist)
  (loop for (name val) on arglist by #'cddr
	unless (member (symbol-name name) key-names
		       :key #'symbol-name :test 'equal)
	  append (list name val)))

(defun mapappend (func seq)
  (apply #'append (mapcar func seq)))

(defun mapcar-append-string-nontailrec (func v)
  "Concatenate the results of a mapcar lambda call, without using tail recursion."
  (aif (car v)
       (concatenate 'string (funcall func it)
		    (mapcar-append-string-nontailrec func (cdr v)))
       ""))

(defun mapcar-append-string (func v &optional (accum ""))
  "Concatenate the results of mapcar lambda calls, using tail recursion."
  (aif (car v)
       (mapcar-append-string
	 func
	 (cdr v)
	 (concatenate 'string accum (funcall func it)))
       accum))

(defun mapcar2-append-string-nontailrec (func la lb)
  "Concatenate the results of mapcar lambda calls over two lists, without using tail recursion."
  (let ((a (car la))
	(b (car lb)))
    (if (and a b)
	(concatenate 'string (funcall func a b)
		     (mapcar2-append-string-nontailrec func (cdr la) (cdr lb)))
	"")))

(defun mapcar2-append-string (func la lb &optional accum)
  "Concatante the results of mapcar lambda calls over two lists, using tail recursion."
  (let ((a (car la))
	(b (car lb)))
    (if (and a b)
	(mapcar2-append-string func (cdr la) (cdr lb)
			       (concatenate 'string accum (funcall func a b)))
	accum)))

(defun append-sublists (lst)
  "Takes a list of lists and appends all sublists."
  (let ((results (car lst)))
    (dolist (elem (cdr lst) results)
      (setq results (append results elem)))))

(defun alist-elem-p (elem)
  (and (consp elem) (atom (car elem)) (atom (cdr elem))))

(defun alistp (alist)
  (when (listp alist)
    (dolist (elem alist)
      (unless (alist-elem-p elem)
	(return-from alistp nil)))
    t))

(defmacro update-alist (akey value alist &key (test '#'eql) (key '#'identity))
  "Macro to support below (setf get-alist)"
  (let ((elem (gensym "ELEM-"))
	(val (gensym "VAL-")))
    `(let ((,elem (assoc ,akey ,alist :test ,test :key ,key))
	   (,val ,value))
       (cond (,elem
	      (setf (cdr ,elem) ,val))
	     (,alist
	      (setf (cdr (last ,alist)) (list (cons ,akey ,val))))
	     (t
	      (setf ,alist (list (cons ,akey ,val)))))
       ,alist)))

(defun get-alist (key alist &key (test #'eql))
  (cdr (assoc key alist :test test)))

(defun (setf get-alist) (value key alist &key (test #'eql))
  "This won't work if the alist is NIL."
  (update-alist key value alist :test test)
  value)

(defun alist-plist (alist)
  (apply #'append (mapcar #'(lambda (x) (list (car x) (cdr x))) alist)))

(defun plist-alist (plist)
  (do ((alist '())
       (pl plist (cddr pl)))
      ((null pl) alist)
  (setq alist (acons (car pl) (cadr pl) alist))))

(defmacro update-plist (pkey value plist &key (test '#'eql))
  "Macro to support (setf get-alist)."
  (let ((pos (gensym)))
    `(let ((,pos (member ,pkey ,plist :test ,test)))
       (if ,pos
	   (progn
	     (setf (cadr ,pos) ,value)
	     ,plist)
	   (setf ,plist (append ,plist (list ,pkey ,value)))))))

(defun unique-slot-values (list slot &key (test #'eql))
  (let ((uniq '()))
    (dolist (item list (nreverse uniq))
      (let ((value (slot-value item slot)))
	(unless (find value uniq :test test)
	  (push value uniq))))))

;; lists.lisp ends here
