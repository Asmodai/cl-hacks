;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; matrix.lisp --- Matrix maths
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:45:38 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 23:27:56
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
(in-package #:cl-hacks)

(defun matrix-minor (row column matrix)
  (let ((result (copy-tree matrix)))
    (if (eql row 1)
        (pop result)
        (setf (cdr (nthcdr (- row 2) result)) (nthcdr row result)))
    (dotimes (current-row-index (length result))
      (let ((current-row (nth current-row-index result)))
        (if (eql column 1)
            (pop (nth current-row-index result))
            (setf (cdr (nthcdr (- column 2) current-row))
                  (nthcdr column current-row)))))
    result))

(defun matrix-rows (matrix)
  (length matrix))

(defun matrix-columns (matrix)
  (length (first matrix)))

(defun matrix-row (n matrix)
  (if (or (> n (matrix-rows matrix))
          (< n 1))
      (error "Invalid row."))
  (list (copy-list (nth (1- n) matrix))))

(defun matrix-column (n matrix)
  (if (or (> n (matrix-columns matrix))
          (< n 1))
      (error "Invalid column."))
  (mapcar #'(lambda (r) (list (nth (1- n) r))) matrix))

(defmacro matrix-element (i j matrix)
  `(nth (1- ,j) (nth (1- ,i) ,matrix)))

(defun matrix-rank (matrix)
  (let ((ref (matrix-ref matrix))
        (rank 0)
        (zero-row (first (matrix-zero 1 (matrix-columns matrix)))))
    (dolist (row ref)
      (if (equalp row zero-row)
          (return-from matrix-rank rank)
          (incf rank)))
    rank))

(defun matrix-augment (matrix-1 matrix-2)
  (if (not (or (eql (matrix-rows matrix-1)
                    (matrix-rows matrix-2))
               (null matrix-1)
               (null matrix-2)))
      (error "Invalid dimensions."))
  (cond
    ((null matrix-1) (copy-tree matrix-2))
    ((null matrix-2) (copy-tree matrix-1))
    (t (let ((result (copy-tree matrix-1)))
         (dotimes (i (matrix-rows matrix-1))
           (setf (nth i result) (nconc (nth i result)
                                       (nth i  matrix-2))))
         result))))

(defun matrix-split (column matrix)
  (if (or (< column 1)
          (> column (matrix-columns matrix)))
      (error "Invalid split point"))
  (let ((matrix-1)
        (matrix-2)
        (split-point (- (matrix-columns matrix) column)))
    (do ((i (matrix-rows matrix) (1- i)))
        ((< i 1))
      (let ((current-row (matrix-row i matrix)))
        (push (butlast (first current-row) split-point) matrix-1)
        (push (last (first current-row) split-point) matrix-2)))
    (values matrix-1 matrix-2)))

(defun matrix-apply (func matrix)
  (let ((result (copy-tree matrix)))
    (dotimes (i (matrix-rows result))
      (dotimes (j (matrix-columns result))
        (let ((i (1+ i)) (j (1+ j)))
          (setf (matrix-element i j result)
                (funcall func (matrix-element i j result))))))
    result))

(defun matrixp (matrix)
  (if (atom matrix)
      (return-from matrixp nil))
  ;; The reduce statement returns the common number of columns or -1
  ;; if not a matrix
  (not (eql (reduce #'(lambda (row-1-length row-2-length)
                        (if (eql row-1-length row-2-length)
                            row-1-length -1))
                    matrix :key #'length) -1)))

(defun matrix-squarep (matrix)
  (eql (matrix-columns matrix) (matrix-rows matrix)))

(defun matrix-upper-triangularp (matrix &key (tolerance 1E-5))
  (dotimes (i (matrix-rows matrix))
    (let ((i (1+ i)))
      (dotimes (j (1- i))
        (let ((j (1+ j)))
          (if (> (abs (matrix-element i j matrix)) tolerance)
              (return-from matrix-upper-triangularp nil))))))
  t)

(defun matrix-symmetricp (matrix)
  (if (not (matrix-squarep matrix))
      (return-from matrix-symmetricp nil))
  (do ((i 1 (1+ i)))
      ((>= i (matrix-rows matrix)))
    (do ((j (1+ i) (1+ j)))
        ((> j (matrix-columns matrix)))
      (if (not (equalp (matrix-element i j matrix)
                       (matrix-element j i matrix)))
          (return-from matrix-symmetricp nil))))
  t)

(defun matrix-orthogonalp (matrix)
  (dotimes (i (matrix-columns matrix))
    (do* ((i (1+ i))
          (j (1+ i) (1+ j))
          (column-i (matrix-transpose (matrix-column i matrix))))
         ((> j (matrix-columns matrix)))
      ;; If dot product is not zero between two different columns
      (if (/= (matrix-element
               1
               1
               (matrix-multiply column-i (matrix-column j matrix))) 0)
          (return-from matrix-orthogonalp nil))))
  t)

(defun matrix-invertiblep (matrix)
  ;; If the determinant is not zero, it is invertible
  (if (matrix-squarep matrix)
      (equalp (simplify (matrix-determinant matrix)) 0)
      nil))

(defun matrix-zero (m n)
  (let ((result nil))
    (dotimes (i m)
      (push (make-list n :initial-element 0) result))
    result))

(defun matrix-ones (m n)
  (let ((result nil))
    (dotimes (i m)
      (push (make-list n :initial-element 1) result))
    result))

(defun matrix-identity (n)
  (let ((result (matrix-zero n n)))
    (dotimes (i n)
      (let ((i (1+ i)))
        (setf (matrix-element i i result) 1)))
    result))

(defun matrix-random (m n &optional (maximum 1e0))
  (let ((matrix nil))
    (dotimes (i m)
      (push nil matrix)
      (dotimes (j n)
        (push (random maximum) (nth 0 matrix))))
    matrix))

(defun matrix-add (matrix-1 matrix-2)
  (if (not (and (eql (matrix-rows matrix-1) (matrix-rows matrix-2))
                (eql (matrix-columns matrix-1) (matrix-columns matrix-2))))
      (error "Invalid dimensions."))
  (let ((result (copy-tree matrix-1)))
    (dotimes (row (matrix-rows result))
      (dotimes (column (matrix-columns result))
        (let ((row (1+ row)) (column (1+ column)))
          (setf (matrix-element row column result)
                (+ (matrix-element row column result)
                   (matrix-element row column matrix-2))))))
    result))

(defun matrix-multiply (matrix-1 matrix-2)
  (cond
    ;; Scalar multiplicaton, scalar first
    ((or (atom matrix-1) (atom (first matrix-1)))
     (if (and (numberp matrix-1)
              (numberp matrix-2))
         (* matrix-1 matrix-2) ; Both scalars
         (matrix-apply #'(lambda (x) (* matrix-1 x)) matrix-2)))
    ;; Scalar multiplication, scalar second
    ((or (atom matrix-2)
         (atom (first matrix-2)))
     (matrix-multiply matrix-2 matrix-1))
    ((not (eql (matrix-columns matrix-1)
               (matrix-rows matrix-2)))
     (error "Invalid dimensions."))
    (t
     (let ((result (matrix-zero (matrix-rows matrix-1)
                                (matrix-columns matrix-2))))
       ;; Perform multiplication, row interpretation used
       (dotimes (i (matrix-rows matrix-1))
         (dotimes (j (matrix-columns matrix-1))
           (let ((i (1+ i)) (j (1+ j)))
             (setf (nth (1- i) result)
                   (first (matrix-add
                           (list (nth (1- i) result))
                           (matrix-multiply (matrix-element i j matrix-1)
                                            (list (nth (1- j) matrix-2)))))))))
       result))))

(defun matrix-normalize (matrix)
  (if (and (/= (matrix-rows matrix) 1)
           (/= (matrix-columns matrix) 1))
      (error "Invalid dimensions."))
  (let ((length-squared))
    (if (eql (matrix-columns matrix) 1)
        (setf length-squared
              (matrix-element 1
                              1
                              (matrix-multiply
                               (matrix-transpose matrix) matrix)))
        (setf length-squared (matrix-element 1
                                             1
                                             (matrix-multiply
                                              matrix
                                              (matrix-transpose matrix)))))
    (if (equalp (simplify length-squared) 0)
        matrix
        (matrix-multiply matrix `(/ 1 (expt ,length-squared 1/2))))))

; Gram-Schmidt method
(defun matrix-qr (matrix)
  (if (not (matrix-squarep matrix))
      (error "Invalid dimensions."))
  (let ((matrix-q nil)
        (matrix-r (matrix-zero (matrix-rows matrix)
                               (matrix-rows matrix))))
    (dotimes (i (matrix-columns matrix))
      ;; Set next-q as the current column of matrix and index as i
      (do ((next-q (matrix-column (1+ i) matrix))
           (current-index i (1- current-index)))
          ;; When index is 0, no more terms to subtract, augment matrix-q
          ((<= current-index 0) 
           (setf matrix-q (matrix-augment matrix-q
                                          (matrix-normalize next-q)))
           ;; This line the diagonal of r, the only terms not
           ;; calculated in Gram-Schmidt
           (setf (matrix-element (1+ i) (1+ i) matrix-r)
                 `(expt ,(matrix-element 1 1
                                         (matrix-multiply
                                          (matrix-transpose next-q)
                                          next-q)) 1/2)))
        ;; Define current-column as the orthonormal vector at
        ;; current-index
        (let ((current-column (matrix-column current-index matrix-q)))
          ;; The following set statement finds q_i-1^T * A_i, or r_i
          (setf (matrix-element current-index (1+ i) matrix-r)
                (simplify (matrix-element 1
                                          1
                                          (matrix-multiply
                                           (matrix-transpose
                                            current-column)
                                           (matrix-column (1+ i) matrix)))))
          ;; The following set statement subtracts one term, so that
          ;; upon the completion of the do loop, q_i is as follows:
          ;; q_i = A_i - (q_i-1^T * A_i) / (q_i-1^T * q_i-1) * q_i-1)
          ;; - (q_i-2^T * A_i) / (q_i-2^T * q_i-2) * q_i-2)
          ;; ...
          ;; - (q_1^T * A_i) / (q_1^T * q_1) * q_1)
          (setf next-q
                (matrix-apply
                 #'simplify
                 (matrix-add
                  next-q
                  (matrix-multiply
                   `(* -1 ,(matrix-element current-index (1+ i) matrix-r))
                   current-column)))))))
    (values matrix-q matrix-r)))

;;; LU Decomposition
;;; Returns multiple values (matrix-l matrix-u)
(defun matrix-lu (matrix)
  (if (not (matrix-squarep matrix))
      (error "Invalid dimensions."))
  (let ((matrix-u nil)
        (matrix-l (matrix-identity (matrix-rows matrix))))
    (dotimes (i (matrix-rows matrix))
      (let* ((i (1+ i)) (next-u (matrix-row i matrix)))
        (do ((current-index 1 (1+ current-index)))
            ((> current-index (matrix-rows matrix-u))
             (setf matrix-u (nconc matrix-u next-u)))
          ;; Store the multiplier of row current-index to be
          ;; subtracted from row i
          (setf (matrix-element i current-index matrix-l)
                `(/ ,(matrix-element 1 current-index next-u)
                    ,(matrix-element current-index current-index matrix-u)))
          ;; Reduce the row
          (setf next-u (matrix-add
                        next-u
                        (matrix-multiply
                         `(* -1 ,(matrix-element i current-index  matrix-l))
                         (matrix-row current-index matrix-u)))))))
    (values matrix-l matrix-u)))

(defun matrix-transpose (matrix)
  (let ((result (matrix-zero (matrix-columns matrix) (matrix-rows matrix))))
    (dotimes (i (matrix-rows result))
      (dotimes (j (matrix-columns result))
        (let ((i (1+ i)) (j (1+ j)))
          (setf (matrix-element i j result)
                (matrix-element j i  matrix)))))
    result))

(defun matrix-count-zeros (matrix)
  (let ((result 0))
    (dotimes (i (matrix-rows matrix))
      (dotimes (j (matrix-columns matrix))
        ;; When simplify is better, use here
        (let ((i (1+ i)) (j (1+ j)))
          (if (eql (matrix-element i j matrix) 0) (incf result)))))
    result))

(defun matrix-determinant (matrix)
  (if (not (matrix-squarep matrix))
      (error "Invalid dimensions."))
  (cond
    ((eql (length matrix) 1) (matrix-element 1 1 matrix))
    (t (let ((result (list '+)) (row-index 1))
         ;; If more than 4 columns, optimize by counting zeros on the
         ;; rows
         (when (> (matrix-columns matrix) 4)
           (let ((most-zeros 0))
             (dotimes (i (matrix-columns matrix))
               (let ((zeros (matrix-count-zeros
                             (matrix-row (1+ i) matrix))))
                 (if (> zeros most-zeros)
                     (setf most-zeros zeros row-index (1+ i)))))))
         (dotimes (column-index (matrix-columns matrix))
           (let ((column-index (1+ column-index)))
             ;; Don't bother finding determinants that will be
             ;; multiplied by 0
             (if (eql (matrix-element row-index column-index matrix) 0)
                 (push 0 result)
                 (push `(* ,(expt -1 (+ column-index row-index))
                           ,(matrix-element row-index column-index matrix)
                           ,(matrix-determinant (matrix-minor
                                                 row-index
                                                 column-index
                                                 matrix))) result))))
         (nreverse result)))))

(defun matrix-row-swap (row-1-index row-2-index matrix)
  ;; Nth is used over matrix-row for efficiency
  (let* ((result (copy-tree matrix))
         (tmp (nth (1- row-1-index) result)))
    (setf (nth (1- row-1-index) result) (nth (1- row-2-index) result))
    (setf (nth (1- row-2-index) result) tmp)
    result))

(defun print-matrix (outputstream matrix &optional colon at)
  ;; Get rid of compile warnings of being unused
  (declare (ignore colon at))
  ;; The inner format creates a format string with a column width
  ;; specified by calculate-column-width
  (let ((width (calculate-column-width matrix)))
    (format outputstream "~& _~vt _~%"
            (* width (matrix-columns matrix)))
    (format outputstream "| ~vt  |~%"
            (* width (matrix-columns matrix)))
    (format outputstream
            (format nil "~~{|~~{~~~a@a~~} |~~%~~}" width) matrix)
    (format outputstream "|_~vt _|~%"
            (* width (matrix-columns matrix)))))

(defun calculate-column-width (matrix)
  (let ((width 0))
    (dolist (row matrix)
      (dolist (element row)
        ;; The nil in the format tells it to include quotes in the
        ;; output
        (let ((element-width (length (format nil "~a" element))))
          (if (> element-width width) (setf width element-width)))))
    (1+ width)))

(defun correct-near-zero (matrix &key (tolerance 1E-5))
  (dotimes (i (matrix-rows matrix))
    (let ((i (1+ i)))
      (dotimes (j (matrix-columns matrix))
        (let ((j (1+ j)))
          (if (<= (abs (matrix-element i j matrix)) tolerance)
              (setf (matrix-element i j matrix) 0))))))
  matrix)

;;; Uses the QR algorithm (unoptimized) to find the eigenvalues
(defun matrix-eigenvalues (matrix &key (max-iterations 40) (tolerance 1E-5))
  (setf matrix (matrix-apply #'eval matrix))
  (do ((iterations 0 (1+ iterations)))
      ((matrix-upper-triangularp matrix :tolerance tolerance)
       (format t "Iterations: ~a~%" iterations))
    (when (> iterations max-iterations)
      (print-matrix t matrix)
      (error "Too many iteratons"))
    (setf matrix (multiple-value-bind (q r) (matrix-qr matrix)
                   (matrix-multiply r q)))
    (setf matrix (correct-near-zero
                  (matrix-apply #'eval matrix) :tolerance tolerance)))
  ;; Load the eigenvalues into a list
  (let ((eigenvalues nil))
    (dotimes (index (matrix-rows matrix))
      (let ((index (1+ index)))
        (push (matrix-element index index matrix) eigenvalues)))
    (sort eigenvalues #'<)))

;;; Row Echelon Form
;;; Thanks to Wikipedia for pseudocode on the Gaussian Elimination pagea
;;; Note: This is not symbolic because it would force assumptions to be made
(defun matrix-ref (matrix)
  (do* ((i 1)
        (j 1 (1+ j))
        (maxi i i)
        (rows (matrix-rows matrix))
        (columns (matrix-columns matrix)))
       ((or (> i rows) (> j columns)) matrix)
    (do ((k (1+ i) (1+ k)))
        ((> k rows))
      (if (> (abs (matrix-element k j matrix))
             (abs (matrix-element maxi j matrix)))
          (setf maxi k)))
    (when (/= (matrix-element maxi j matrix) 0)
      (setf matrix (matrix-row-swap i maxi matrix))
      (do ((k 1 (1+ k))
           (divisor (matrix-element i j matrix)))
          ((> k columns))
        (setf (matrix-element i k matrix)
              (/ (matrix-element i k matrix) divisor)))
      (do ((u (1+ i) (1+ u)))
          ((> u rows))
        (do ((k 1 (1+ k))
             (multiplier (matrix-element u j matrix)))
            ((> k columns))
          (let ((value (* (matrix-element i k matrix) multiplier)))
            (setf (matrix-element u k matrix)
                  (- (matrix-element u k matrix) value)))))
      (incf i))))

;;; Reduced row echelon form
;;; Thanks to Wikipedia for pseudocode on the rref page
;;; Note: This is not symbolic because it would force assumptions to be made. 
;;; We all know what happens when you make assumptions.
(defun matrix-rref (matrix)
  (do* ((r 1 (1+ r))
        (i r r)
        (lead 1 (1+ lead))
        (rows (matrix-rows matrix))
        (columns (matrix-columns matrix)))
       ((> r rows) matrix)
    (if (< columns lead)
        (return-from matrix-rref matrix))
    (do ()
        ((/= (matrix-element i lead matrix) 0))
      (incf i)
      (when (< rows i)
        (setf i r)
        (incf lead)
        (if (< columns lead)
            (return-from matrix-rref matrix))))
    (setf matrix (matrix-row-swap i r matrix))
    (do ((j 1 (1+ j))
         (divisor (matrix-element r lead matrix)))
        ((> j columns))
      (setf (matrix-element r j matrix)
            (/ (matrix-element r j matrix) divisor)))
    (do ((j 1 (1+ j)))
        ((> j rows))
      (if (/= j r)
          (do ((k 1 (1+ k))
               (multiplier (matrix-element j lead matrix)))
              ((> k columns))
            (let ((value (* multiplier (matrix-element r k matrix))))
              (setf (matrix-element j k matrix)
                    (- (matrix-element j k matrix) value))))))))

;;; Guass-Jordan elimination to solve Ax=b, for a square A
(defun matrix-gauss-jordan (matrix-a matrix-b)
  (if (not (matrix-squarep matrix-a))
      (error "Invalid dimensions."))
  (let ((m (matrix-columns matrix-a)))
    (multiple-value-bind (matrix-1 matrix-2)
        (matrix-split m (matrix-rref (matrix-augment matrix-a matrix-b)))
      (if (not (equalp (matrix-identity m) matrix-1))
          (error "Singular matrix") matrix-2))))

;;; Guassian elimination with back substitution to solve Ax=b, b a
;;; column vector. The optional solution parameter is for known
;;; values, such as the ones used to generate the nullspace.
(defun matrix-gaussian (matrix-a matrix-b &optional
                        (solution (make-list (matrix-columns matrix-a))))
  (matrix-gaussian2 (matrix-ref (matrix-augment matrix-a matrix-b)) solution))

;;; This function drives the Gaussian elimination, it's purpose is to
;;; take a ref matrix and part of a known (or completely unknown)
;;; solution and solve Ax=b for b a column vector
(defun matrix-gaussian2 (ref solution)
  ;; Reverse ref to make the back substitution easier
  (let* ((ref (nreverse ref))
         (columns (matrix-columns ref))
         (parts-found 0))
    (dolist (i solution)
      (if (not (null i))
          (incf parts-found)))
    (dolist (row ref)
      (let ((nonzeros (- columns (matrix-count-zeros (list row)))))
        (if (> nonzeros 0)
            ;; + 2 because this allows for a nonzero in b and in A
            (cond
              ((> nonzeros (+ 2 parts-found))
               (error "Singular matrix"))
              ((and (= nonzeros 1) (/= (car (last row)) 0))
               (error "Singular matrix"))
              ((= nonzeros 1)
               ;; 1- columns to not include b
               (dotimes (i (1- columns))
                 (when (/= (nth i row) 0)
                   (setf (nth i solution) 0)
                   (incf parts-found)
                   (return))))
              (t
               (let ((sum 0) (solve-for))
                 (dotimes (i (1- columns))
                   (if (/= (nth i row) 0)
                       (if (not (null (nth i solution)))
                           (setf sum (+ sum (* (nth i row) (nth i solution))))
                           (setf solve-for i))))
                 ;; Subtract sum from rhs, divide by coefficient, and
                 ;; store as part of solution
                 (if (null solve-for)
                     (error "Singular matrix"))
                 (setf (nth solve-for solution)
                       (/ (- (car (last row)) sum)
                          (nth solve-for row)))
                 (incf parts-found)))))))
    ;; Replace any remaining nils with 0
    (if (/= parts-found (length solution))
        (setf solution (nsubstitute 0 nil solution)))
    (matrix-transpose (list solution))))

;;; Take the inverse using Gauss-Jordan elimination
(defun matrix-inverse (matrix)
  (matrix-gauss-jordan matrix (matrix-identity (matrix-columns matrix))))

;;; Helper for nullspace and columnspace, generates a row of zeros of
;;; length columns and a one at the index one-column
(defun standard-basis-row (one-column columns)
  (let ((row (make-list columns :initial-element 0)))
    (setf (nth (1- one-column) row) 1)
    (list row)))

;;; Find the nullspace of matrix
(defun matrix-nullspace (matrix)
  (let ((rref (matrix-rref matrix))
        (nullspace)
        (free-column-indicies)
        (columns (matrix-columns matrix))
        (pivot-count 0))
    ;; Find the free columns
    (dotimes (i (matrix-rows rref))
      (if (not (equal (standard-basis-row (1+ pivot-count) columns)
                      (matrix-transpose (matrix-column (1+ i) rref))))
          (push i free-column-indicies) (incf pivot-count)))
    ;; Augment a zero column onto rref
    (setf rref (matrix-augment rref (matrix-zero (matrix-rows matrix) 1)))
    ;; Iterate through the free columns setting one to 1 and the
    ;; others to 0 each time
    (dolist (i free-column-indicies)
      (let ((solution (make-list (matrix-columns matrix))))
        (dolist (j free-column-indicies)
          (setf (nth j solution) (if (/= i j) 0 1)))
        ;; Use Gaussian elimination to find the nullspace given the
        ;; free variables in solution.  The gaussian2 function is used
        ;; to avoid repeated calls to ref
        (setf nullspace (matrix-augment
                         nullspace
                         (matrix-gaussian2 rref solution)))))
    nullspace))

;;; Find the columnspace
(defun matrix-columnspace (matrix)
  (let ((rref (matrix-rref matrix))
        (columnspace)
        (pivot-column-indicies)
        (columns (matrix-columns matrix)))
    ;; Identify the independent columns
    (dotimes (i (matrix-rows rref))
      (let ((i (1+ i)))
        (if (equalp (standard-basis-row
                     (1+ (length pivot-column-indicies)) columns)
                    (matrix-transpose (matrix-column i rref)))
            (push i pivot-column-indicies))))
    ;; Build columnspace using the columns
    (dolist (i pivot-column-indicies)
      (setf columnspace (matrix-augment columnspace (matrix-column i matrix))))
    columnspace))

;;; Print C(A), C(A^T), N(A), and N(A^T)
(defun print-matrix-four-subspaces (matrix)
  (format t "Columnspace: ~/print-matrix/~%"
          (matrix-columnspace matrix))
  (format t "Left nullspace: ~/print-matrix/~%"
          (matrix-nullspace (matrix-transpose matrix)))
  (format t "Rowspace: ~/print-matrix/~%"
          (matrix-columnspace (matrix-transpose matrix)))
  (format t "Nullspace: ~/print-matrix/~%"
          (matrix-nullspace matrix)))

;;; matrix.lisp ends here
