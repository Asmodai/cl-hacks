;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; sequences.lisp --- Sequences
;;;
;;; Time-stamp: <Monday Dec  5, 2011 05:06:03 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Dec 2011 03:16:20
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

(declaim (inline copy-sequence sequence-of-length-p))

(defun rotate-tail-to-head (sequence n)
  (declare (type (integer 1) n))
  (if (listp sequence)
      (let ((m (mod n (list-length sequence))))
        (if (null (cdr sequence))
            sequence
            (let* ((tail (last sequence (+ m 1)))
                   (last (cdr tail)))
              (setf (cdr tail) nil)
              (nconc last sequence))))
      (let* ((len (length sequence))
             (m (mod n len))
             (tail (subseq sequence (- len m))))
        (replace sequence sequence :start1 m :start2 0)
        (replace sequence tail)
        sequence)))

(defun rotate-head-to-tail (sequence n)
  (declare (type (integer 1) n))
  (if (listp sequence)
      (let ((m (mod (1- n) (list-length sequence))))
        (if (null (cdr sequence))
            sequence
            (let* ((headtail (nthcdr m sequence))
                   (tail (cdr headtail)))
              (setf (cdr headtail) nil)
              (nconc tail sequence))))
      (let* ((len (length sequence))
             (m (mod n len))
             (head (subseq sequence 0 m)))
        (replace sequence sequence :start1 0 :start2 m)
        (replace sequence head :start1 (- len m))
        sequence)))

(defun rotate (sequence &optional (n 1))
  (if (plusp n)
      (rotate-tail-to-head sequence n)
      (if (minusp n)
          (rotate-head-to-tail sequence (- n))
          sequence)))

(defun shuffle (sequence &key (start 0) end)
  (declare (fixnum start)
           (type (or fixnum null) end))
  (typecase sequence
    (list
     (let* ((end (or end (list-length sequence)))
            (n (- end start)))
       (do ((tail (nthcdr start sequence) (cdr tail)))
           ((zerop n))
         (rotatef (car tail) (car (nthcdr (random n) tail)))
         (decf n))))
    (vector
     (let ((end (or end (length sequence))))
       (loop for i from (- end 1) downto start
             do (rotatef (aref sequence i)
                         (aref sequence (random (+ i 1)))))))
    (sequence
     (let ((end (or end (length sequence))))
       (loop for i from (- end 1) downto start
             do (rotatef (elt sequence i)
                         (elt sequence (random (+ i 1))))))))
  sequence)

(defun random-elt (sequence &key (start 0) end)
  (declare (sequence sequence)
           (fixnum start)
           (type (or fixnum null) end))
  (let ((i (+ start (random (- (or end  (if (listp sequence)
                                            (list-length sequence)
                                            (length sequence)))
                               start)))))
    (elt sequence i)))

(defsubst remove/swapped-arguments (sequence item
                                    &rest keyword-arguments)
  (apply #'remove item sequence keyword-arguments))

(define-modify-macro removef (item &rest remove-keywords)
  remove/swapped-arguments
  "Modify-macro for REMOVE. Sets place designated by the first
argument to the result of calling REMOVE with ITEM, place, and the
REMOVE-KEYWORDS.")

(defsubst delete/swapped-arguments (sequence item
                                    &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))

(define-modify-macro deletef (item &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first
argument to the result of calling DELETE with ITEM, place, and the
REMOVE-KEYWORDS.")

(deftype proper-sequence ()
   `(or proper-list
       (and (not list) sequence)))

(defun emptyp (sequence)
  (etypecase sequence
    (list (null sequence))
    (sequence (zerop (length sequence)))))

(defun length= (&rest sequences)
  (declare (dynamic-extent sequences)
           (inline sequence-of-length-p)
           (optimize speed))
  (unless (cdr sequences)
    (error "You must call LENGTH= with at least two arguments"))
  ;; There's room for optimization here: multiple list arguments could
  ;; be traversed in parallel.
  (let* ((first (pop sequences))
         (current (if (integerp first)
                      first
                      (length first))))
    (declare (type array-index current))
    (dolist (el sequences)
      (if (integerp el)
          (unless (= el current)
            (return-from length= nil))
          (unless (sequence-of-length-p el current)
            (return-from length= nil)))))
  t)

(define-compiler-macro length= (&whole form length &rest sequences)
  (cond
    ((zerop (length sequences))
     form)
    (t
     (let ((optimizedp (integerp length)))
       (with-unique-names (tmp current)
         (declare (ignorable current))
         `(locally
              (declare (inline sequence-of-length-p))
            (let ((,tmp)
                  ,@(unless optimizedp
                      `((,current ,length))))
              ,@(unless optimizedp
                  `((unless (integerp ,current)
                      (setf ,current (length ,current)))))
              (and
               ,@(loop
                   :for sequence :in sequences
                   :collect `(progn
                               (setf ,tmp ,sequence)
                               (if (integerp ,tmp)
                                   (= ,tmp ,(if optimizedp
                                                length
                                                current))
                                   (sequence-of-length-p
                                    ,tmp
                                    ,(if optimizedp
                                         length
                                         current)))))))))))))

(defun sequence-of-length-p (sequence length)
  (declare (type array-index length)
           (inline length)
           (optimize speed))
  (etypecase sequence
    (null
     (zerop length))
    (cons
     (let ((n (1- length)))
       (unless (minusp n)
         (let ((tail (nthcdr n sequence)))
           (and tail
                (null (cdr tail)))))))
    (vector
     (= length (length sequence)))
    (sequence
     (= length (length sequence)))))

(defun copy-sequence (type sequence)
  (if (typep sequence type)
      (copy-seq sequence)
      (coerce sequence type)))

(defun first-elt (sequence)
  ;; Can't just directly use ELT, as it is not guaranteed to signal
  ;; the type-error.
  (cond ((consp sequence)
         (car sequence))
        ((and (typep sequence '(and sequence (not list)))
              (plusp (length sequence)))
         (elt sequence 0))
        (t
         (error 'type-error
                :datum sequence
                :expected-type '(and sequence
                                 (not (satisfies emptyp)))))))

(defun (setf first-elt) (object sequence)
  ;; Can't just directly use ELT, as it is not guaranteed to signal
  ;; the type-error.
  (cond ((consp sequence)
         (setf (car sequence) object))
        ((and (typep sequence '(and sequence (not list)))
              (plusp (length sequence)))
         (setf (elt sequence 0) object))
        (t
         (error 'type-error
                :datum sequence
                :expected-type '(and sequence
                                 (not (satisfies emptyp)))))))

(defun last-elt (sequence)
  ;; Can't just directly use ELT, as it is not guaranteed to signal
  ;; the type-error.
  (let ((len 0))
    (cond ((consp sequence)
           (lastcar sequence))
          ((and (typep sequence '(and sequence (not list)))
                (plusp (setf len (length sequence))))
           (elt sequence (1- len)))
          (t
           (error 'type-error
                  :datum sequence
                  :expected-type '(and proper-sequence
                                   (not (satisfies emptyp))))))))

(defun (setf last-elt) (object sequence)
  (let ((len 0))
    (cond ((consp sequence)
           (setf (lastcar sequence) object))
          ((and (typep sequence '(and sequence (not list)))
                (plusp (setf len (length sequence))))
           (setf (elt sequence (1- len)) object))
          (t
           (error 'type-error
                  :datum sequence
                  :expected-type '(and proper-sequence
                                   (not (satisfies emptyp))))))))

(defun starts-with-subseq (prefix sequence
                           &rest args
                           &key (return-suffix nil) &allow-other-keys)
  (remove-from-plistf args :return-suffix)
  (let ((sequence-length (length sequence))
        (prefix-length (length prefix)))
    (if (<= prefix-length sequence-length)
        (let ((mismatch (apply #'mismatch sequence prefix args)))
          (if mismatch
              (if (< mismatch prefix-length)
                  (values nil nil)
                  (values
                   t
                   (when return-suffix
                     (make-array (- sequence-length mismatch)
                                 :element-type
                                 (array-element-type sequence)
                                 :displaced-to sequence
                                 :displaced-index-offset prefix-length
                                 :adjustable nil))))
              (values t (when return-suffix
                          (make-array 0 :element-type
                                      (array-element-type sequence)
                                      :adjustable nil)))))
        (values nil nil))))

(defun ends-with-subseq (suffix sequence &key (test #'eql))
  (let ((sequence-length (length sequence))
        (suffix-length (length suffix)))
    (when (< sequence-length suffix-length)
      ;; if SEQUENCE is shorter than SUFFIX, then SEQUENCE can't end
      ;; with SUFFIX.
      (return-from ends-with-subseq nil))
    (loop for sequence-index
            from (- sequence-length suffix-length)
              below sequence-length
          for suffix-index from 0 below suffix-length
          when (not (funcall test (elt sequence sequence-index)
                                  (elt suffix suffix-index)))
            do (return-from ends-with-subseq nil)
          finally (return t))))

(defun starts-with (object sequence
                    &key (test #'eql) (key #'identity))
  (funcall test
           (funcall key
                    (typecase sequence
                      (cons (car sequence))
                      (sequence
                       (if (plusp (length sequence))
                           (elt sequence 0)
                           (return-from starts-with nil)))
                      (t
                       (return-from starts-with nil))))
           object))

(defun ends-with (object sequence
                  &key (test #'eql) (key #'identity))
  (funcall test
           (funcall key
                    (typecase sequence
                      (cons
                       ;; signals for improper lists
                       (lastcar sequence))
                      (sequence
                       ;; Can't use last-elt, as that signals an error
                       ;; for empty sequences
                       (let ((len (length sequence)))
                         (if (plusp len)
                             (elt sequence (1- len))
                             (return-from ends-with nil))))
                      (t
                       (return-from ends-with nil))))
           object))

(defun map-combinations (function sequence
                         &key (start 0) end length (copy t))
  (let* ((end (or end (length sequence)))
         (size (- end start))
         (length (or length size))
         (combination (subseq sequence 0 length))
         (function (ensure-function function)))
    (if (= length size)
        (funcall function combination)
        (flet ((call ()
                 (funcall function (if copy
                                       (copy-seq combination)
                                       combination))))
          (etypecase sequence
            ;; When dealing with lists we prefer walking back and
            ;; forth instead of using indexes.
            (list
             (labels ((combine-list (c-tail o-tail)
                        (if (not c-tail)
                            (call)
                            (do ((tail o-tail (cdr tail)))
                                ((not tail))
                              (setf (car c-tail) (car tail))
                              (combine-list (cdr c-tail)
                                            (cdr tail))))))
               (combine-list combination (nthcdr start sequence))))
            (vector
             (labels ((combine (count start)
                        (if (zerop count)
                            (call)
                            (loop for i from start below end
                                  do (let ((j (- count 1)))
                                       (setf (aref combination j)
                                             (aref sequence i))
                                       (combine j (+ i 1)))))))
               (combine length start)))
            (sequence
             (labels ((combine (count start)
                        (if (zerop count)
                            (call)
                            (loop for i from start below end
                                  do (let ((j (- count 1)))
                                       (setf (elt combination j)
                                             (elt sequence i))
                                       (combine j (+ i 1)))))))
               (combine length start)))))))
  sequence)

(defun map-permutations (function sequence
                         &key (start 0) end length (copy t))
  (let* ((end (or end (length sequence)))
         (size (- end start))
         (length (or length size)))
    (labels ((permute (seq n)
               (let ((n-1 (- n 1)))
                 (if (zerop n-1)
                     (funcall function (if copy
                                           (copy-seq seq)
                                           seq))
                     (loop for i from 0 upto n-1
                           do (permute seq n-1)
                              (if (evenp n-1)
                                  (rotatef (elt seq 0)
                                           (elt seq n-1))
                                  (rotatef (elt seq i)
                                           (elt seq n-1)))))))
             (permute-sequence (seq)
               (permute seq length)))
      (if (= length size)
          ;; Things are simple if we need to just permute the
          ;; full START-END range.
          (permute-sequence (subseq sequence start end))
          ;; Otherwise we need to generate all the combinations
          ;; of LENGTH in the START-END range, and then permute
          ;; a copy of the result: can't permute the combination
          ;; directly, as they share structure with each other.
          (let ((permutation (subseq sequence 0 length)))
            (flet ((permute-combination (combination)
                     (permute-sequence
                      (replace permutation combination))))
              (declare (dynamic-extent #'permute-combination))
              (map-combinations #'permute-combination sequence
                                :start start
                                :end end
                                :length length
                                :copy nil)))))))

(defun map-derangements (function sequence
                         &key (start 0) end (copy t))
  (let* ((end (or end (length sequence)))
         (size (- end start))
         ;; We don't really care about the elements here.
         (derangement (subseq sequence 0 size))
         ;; Bitvector that has 1 for elements that have been deranged.
         (mask (make-array size
                           :element-type 'bit
                           :initial-element 0)))
    (declare (dynamic-extent mask))
    ;; ad hoc algorith
    (labels ((derange (place n)
               ;; Perform one recursive step in deranging the
               ;; sequence: PLACE is index of the original sequence
               ;; to derange to another index, and N is the number of
               ;; indexes not yet deranged.
               (if (zerop n)
                   (funcall function (if copy
                                         (copy-seq derangement)
                                         derangement))
                   ;; Itarate over the indexes I of the subsequence to
                   ;; derange: if I != PLACE and I has not yet been
                   ;; deranged by an earlier call put the element from
                   ;; PLACE to I, mark I as deranged, and recurse,
                   ;; finally removing the mark.
                   (loop for i from 0 below size
                         do
                         (unless (or (= place (+ i start))
                                     (not (zerop (bit mask i))))
                           (setf (elt derangement i)
                                 (elt sequence place)
                                 (bit mask i) 1)
                           (derange (1+ place) (1- n))
                           (setf (bit mask i) 0))))))
      (derange start size)
      sequence)))

(defun nsubseq (seq start &optional end)
  (unless end (setq end (length seq)))
  (make-array (- end start)
              :element-type (array-element-type seq)
              :displaced-to seq
              :displaced-index-offset start))

(declaim (notinline sequence-of-length-p))

;;; sequences.lisp ends here
