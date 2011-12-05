;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; math.lisp --- Various math stuff
;;;
;;; Time-stamp: <Monday Dec  5, 2011 05:07:22 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 23:27:08
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

(defun primep (n)
  (cond ((minusp n)
         (primep (- n)))
        ((= 1 n)
         nil)
        ((member n '(2 3 5 7))
         t)
        ((evenp n)
         nil)
        (t
         (loop with root = (isqrt n)
               with divisors = (loop for i from 3 to root by 2
                                     collect i)
               for d = (pop divisors)
               if (zerop (mod n d))
                 do (return nil)
               else
                 do (setf divisors (delete-if #'(lambda (x)
                                                  (zerop (mod x d)))
                                              divisors))
               while divisors
               finally (return t)))))

(defun coprime (a b)
  (= 1 (gcd a b)))

(defun totient-phi (m)
  (loop for r from 1 below m
        when (coprime r m)
          count 1))

(defun compute-primes-to (n)
  (cond ((< n 2)
         #())
        ((= n 2)
         #(2))
        ((= n 3)
         #(2 3))
        (t
         (let (bits-max
               bits
               bit
               (prime-count 2)
               (cur-prime 3)
               primes)
           (setf n (- n (if (oddp n)
                            3
                            2)))
           (setf bits-max (/ n 2))
           (setf bits (make-array (list bits-max)
                                  :initial-element 1
                                  :element-type 'bit))
           (loop while (< cur-prime n)
                 do (setf bit (+ cur-prime (/ (- cur-prime 3) 2)))
                    (loop while (< bit bits-max)
                          do (setf (aref bits bit) 0)
                             (incf bit cur-prime))
                    (setf bit (1+ (/ (- cur-prime 3) 2)))
                    (setf bit (position 1 bits :start bit))
                    (if bit
                        (setf cur-prime (+ bit bit 3)
                              prime-count (1+ prime-count))
                        (setf cur-prime n)))
           (setf primes (make-array (list prime-count)
                                    :element-type 'integer))
           (let ((curnum 0))
             (setf (aref primes curnum) 2)
             (incf curnum)
             (setf (aref primes curnum) 3)
             (incf curnum)
             (setf cur-prime 3)
             (setf bit 0)
             (setf bit (position 1 bits :start (1+ bit)))
             (loop while bit
                   do (setf cur-prime (+ bit bit 3))
                      (setf (aref primes curnum) cur-prime)
                      (incf curnum)
                      (setf bit (position 1 bits :start (1+ bit)))))
           primes))))

(defun factorize (n &optional (primes nil))
  (let ((primes (or primes
                    (compute-primes-to (1+ (isqrt n)))))
        (factors '())
        (prime-idx 0))
    (unless (integerp n)
      (error "FACTORIZE was not passed an integer value."))
    (when (< n 0)
      (push -1 factors)
      (setf n (- n)))
    (loop while (and (< prime-idx (length primes))
                     (< 1 n))
          do (let ((prime (elt primes prime-idx))
                   (expo 0))
               (multiple-value-bind (q r)
                   (truncate n prime)
                 (loop while (zerop r)
                       do (incf expo)
                          (setf n q)
                          (multiple-value-setq (q r)
                              (truncate n prime))))
               (when (plusp expo)
                 (push (if (= 1 expo)
                           prime
                           (list 'expt prime expo))
                       factors)))
             (incf prime-idx))
    (when (< 1 n)
      (push n factors))
    (cons '* factors)))

(defun prime-factors (n)
  (mapcan #'(lambda (factor)
              (cond ((and (listp factor)
                          (eql 'expt (first factor)))
                     (destructuring-bind (expt prime expo)
                         factor
                       (declare (ignore expt))
                       (make-list expo :initial-element prime)))
                    (t
                     (list factor))))
          (nreverse (cdr (factorize n)))))

(defun prime-factors* (n)
  (mapcar #'(lambda (factor)
              (cond ((and (listp factor)
                          (eql 'expt (first factor)))
                     (destructuring-bind (expt prime expo)
                         factor
                       (declare (ignore expt))
                       (list prime expo)))
                    (t
                     (list factor 1))))
          (nreverse (cdr (factorize n)))))

(defun goldbach (n)
  (check-type n (integer 4))
  (assert (evenp n))
  (let ((primes (compute-primes-to n))
        (bits (make-array (1+ n)
                          :element-type 'bit
                          :initial-element 0)))
    (loop for p across primes
          do (setf (aref bits p) 1))
    (loop for p across primes
          when (plusp (aref bits (- n p)))
            do (return (list p (- n p)))
          finally (return '()))))

(defun deriv (f dx)
  #'(lambda (x)
      (/ (- (funcall f (+ x dx)) (funcall f x))
         dx)))

(defun sin^ (x)
  (funcall (deriv #'sin 1d-8) x))

(defmacro ensure-integer (obj)
  `(if (stringp ,obj)
       (parse-integer ,obj)
       ,obj))

(defun make-histogram (sequence &key (key (function identity)))
  (let ((table (make-hash-table)))
    (map nil #'(lambda (item)
                 (incf (gethash (funcall key item) table 0)))
         sequence)
    table))

(defun histogram (v n-bins &key min max)
  (declare (fixnum n-bins))
  (when (listp v)
    (setq v (coerce v 'vector)))
  (when (zerop (length v))
    (return-from histogram (values nil nil nil )))
  (let ((n (length v))
        (bins (make-array n-bins
                          :element-type 'integer
                          :initial-element 0))
        found-min found-max)
    (declare (fixnum n))
    (unless (and min max)
      (setq found-min (aref v 0)
            found-max (aref v 0))
      (loop for i fixnum from 1 to (1- n)
            do (let ((x (aref v i)))
                 (cond ((> x found-max)
                        (setq found-max x))
                       ((< x found-min)
                        (setq found-min x)))))
      (unless min
        (setq min found-min))
      (unless max
        (setq max found-max)))
    (let ((width (/ (- max min) n-bins)))
      (setq width (+ width (* double-float-epsilon width)))
      (dotimes (i n)
        (let ((bin (nth-value 0 (truncate (- (aref v i) min) width))))
          (declare (fixnum bin))
          (when (and (not (minusp bin))
                     (< bin n-bins))
            (incf (aref bins bin))))))
    (values bins min max)))

(defun fixnum-width ()
  (nth-value 0 (truncate (+ (/ (log (1+ most-positive-fixnum)) (log 2)) .5))))

(defun scaled-epsilon (float &optional (operation '+))
  (multiple-value-bind (significand exponent)
      (decode-float float)
    (multiple-value-bind (1.0-significand 1.0-exponent)
        (decode-float (float 1.0 float))
      (if (and (eq operation '-)
               (= significand 1.0-significand))
          (scale-float (typecase float
                         (short-float short-float-negative-epsilon)
                         (single-float single-float-negative-epsilon)
                         (double-float double-float-negative-epsilon)
                         (long-float long-float-negative-epsilon))
                       (- exponent 1.0-exponent))
          (scale-float (typecase float
                         (short-float short-float-epsilon)
                         (single-float single-float-epsilon)
                         (double-float double-float-epsilon)
                         (long-float long-float-epsilon))
                       (- exponent 1.0-exponent))))))

(defun sinc (x)
  (if (zerop x)
      1d0
      (let ((x (coerce x 'double-float)))
        (/ (sin x) x))))

(defun numbers-within-percentage (a b percent)
  (let ((abs-diff (* 0.01 percent 0.5 (+ (abs a) (abs b)))))
    (< (abs (- a b)) abs-diff)))

(defun print-float-units (val unit &optional (stream nil))
  (cond ((< val 1d-6) (format stream "~,2,9f nano~a" val unit))
        ((< val 1d-3) (format stream "~,2,6f micro~a" val unit))
        ((< val 1)    (format stream "~,2,3f mili~a" val unit))
        ((> val 1d9)  (format stream "~,2,-9f giga~a" val unit))
        ((> val 1d6)  (format stream "~,2,-6f mega~a" val unit))
        ((> val 1d3)  (format stream "~,2,-3f kilo~a" val unit))
        (t            (format stream "~,2f ~a" val unit))))




(defun simplify (expression)
        (if (atom expression) (return-from simplify expression))
        (if (eql (length expression) 1) (return-from simplify (first expression)))
        (setf expression (copy-list expression))
        (dotimes (i (length expression))
                (if (listp (nth i expression))
                        (setf (nth i expression) (simplify (nth i expression)))))
        (let ((operation (first expression)) (u (second expression)) (v (cddr expression)))
                (cond
                        ((eql operation '-) (add `(,u ,@(negate v))))
                        ((eql operation '/) (multiply `(,u ,@(reciprocal v))))
                        ((eql operation 'sqrt) (exponentiate `(,u 1/2)))
                        ((eql operation '+) (add (cdr expression)))
                        ((eql operation '*) (multiply (cdr expression)))
                        ((eql operation 'expt) (exponentiate (cdr expression)))
                        ((eql operation 'log) (logarithm (cdr expression)))
                        (t expression))))

;(defun solve (expression-1 expression-2)


; Compare terms for ordering
(defun compare-terms (term-1 term-2)
        (cond
                ((or (eql term-1 '+) (eql term-1 '-) (eql term-1 '*) (eql term-1 '/)) t)
                ((or (eql term-2 '+) (eql term-2 '-) (eql term-2 '*) (eql term-2 '/)) nil)
                ((and (numberp term-1) (numberp term-2)) (< term-1 term-2))
                ((numberp term-1) t)
                ((numberp term-2) nil)
                ((atom term-1) t)
                ((atom term-2) nil)
                ((and (eql (first term-1) 'expt) (eql (first term-2) 'expt) (equalp (second term-1) (second term-2))) t)
                (t nil)))

; Order terms
(defun order (expression)
        (cond
                ((atom expression) expression)
                (t (dotimes (i (length expression))
                                 (let ((element (nth i expression)))
                                         (if (not (and (listp element) (not-sortablep (first element))))
                                                 (setf (nth i expression) (order (nth i expression))))))
                         (setf expression (sort expression #'compare-terms))
                         expression)))

; Checks to see if expression is not-sortable
(defun not-sortablep (expression)
        (case expression
                ((expt sqrt sin cos log) t)
                (otherwise nil)))

(defmacro update-terms (value old-value i j terms)
        `(setf (nth ,i ,terms) ,value ,terms (delete ,old-value ,terms :start ,j :end (1+ ,j)) ,j ,i))

; Add a list of terms, no operation included
(defun add (terms)
        (setf terms (order terms))
        (do ((i 0 (1+ i)))
                ((>= i (length terms)) (if (> (length terms) 1) `(+ ,@terms) (first terms)))
                (do* ((j (1+ i) (1+ j)) (u (nth i terms) (nth i terms)) (v (nth j terms) (nth j terms)))
                        ((>= j (length terms)))
                        ;(format t "+Terms:~a~%" terms)
                        (cond
                                ; Next two lines, zero identity of addition
                                ((equalp u 0) (update-terms v v i j terms))
                                ((equalp v 0) (update-terms u v i j terms))
                                ; If just numbers, evaluate
                                ((and (numberp u) (numberp v)) (update-terms (+ u v) v i j terms))
                                ; Next two lines collapse nested addition
                                ((and (listp v) (eql (first v) '+)) (update-terms (add `(,u ,@(cdr v))) v i j terms))
                                ((and (listp u) (eql (first u) '+)) (update-terms (add `(,v ,@(cdr u))) v i j terms))
                                ; If u and v are the same, group them into multiplication
                                ((equalp u v) (update-terms (multiply `(2 ,u)) v i j terms))
                                ; Next two lines, same as above, except u or v can have a multiplier
                                ((and (listp v) (eql (first v) '*) (equalp u (third v))) (update-terms (multiply `(,(add (list 1 (second v))) ,u)) v i j terms))
                                ((and (listp u) (eql (first u) '*) (equalp v (third u))) (update-terms (multiply `(,(add (list 1 (second u))) ,v)) v i j terms))
                                ; Same as above, except u and v can have a multiplier
                                ((and (listp u) (listp v) (eql (first u) '*) (eql (first v) '*) (equalp (third u) (third v))) (update-terms (multiply `(,(add (list (second u) (second v))) ,(third u))) v i j terms))
                                ; log A + log B = log A*B
                                ((and (listp u) (listp v) (eql (first u) 'log) (eql (first v) 'log))
                                 (cond
                                         ; Both logs have implied bases
                                         ((= 2 (length v) (length u)) (update-terms (logarithm (list (multiply `(,(second u) ,(second v))))) v i j terms))
                                         ; The next two lines are if one has an implied base
                                         ((and (eql (length u) 3) (eql (length v) 2) (eql (third u) 'e)) (update-terms (logarithm (list (multiply `(,(second u) ,(second v))))) v i j terms))
                                         ((and (eql (length v) 3) (eql (length u) 2) (eql (third v) 'e)) (update-terms (logarithm (list (multiply `(,(second u) ,(second v))))) v i j terms))
                                         ; No implied bases, but they are the same
                                         ((and (= 3 (length v) (length u)) (equalp (third v) (third u))) (update-terms (logarithm `(,(multiply (list (second u) (second v))) ,(third u)))  v i j terms))))))))

; Multiply a list of terms, no operations included
(defun multiply (terms)
        (setf terms (order terms))
        (do ((i 0 (1+ i)))
                ((>= i (length terms)) (if (> (length terms) 1) `(* ,@terms) (first terms)))
                (do* ((j (1+ i) (1+ j)) (u (nth i terms) (nth i terms)) (v (nth j terms) (nth j terms)))
                        ((>= j (length terms)))
                        ;(format t "*Terms:~a~%" terms)
                        (cond
                                ; Multiply by zero
                                ((or (equalp v 0) (equalp u 0)) (update-terms 0 v i j terms))
                                ; Next two lines, multiply by one
                                ((equalp u 1) (update-terms v v i j terms))
                                ((equalp v 1) (update-terms u v i j terms))
                                ; If u and v are numbers, just multiply
                                ((and (numberp u) (numberp v)) (update-terms (* u v) v i j terms))
                                ; Next two lines collapse multiplication
                                ((and (listp v) (eql (first v) '*)) (update-terms (multiply `(,u ,@(cdr v))) v i j terms))
                                ((and (listp u) (eql (first u) '*)) (update-terms (multiply `(,v ,@(cdr u))) v i j terms))
                                ; If u and v are the same, group as exponent
                                ((equalp u v) (update-terms (exponentiate `(,u 2)) v i j terms))
                                ; Next two lines are the same as above, except u or v can be an exponent
                                ((and (listp u) (eql (first u) 'expt) (equalp (second u) v)) (update-terms (exponentiate `(,v ,(add (list (third u) 1)))) v i j terms))
                                ((and (listp v) (eql (first v) 'expt) (equalp (second v) u)) (update-terms (exponentiate `(,u ,(add (list (third v) 1)))) v i j terms))
                                ; Same as previous two lines, except u and v are exponents
                                ((and (listp u) (listp v) (eql (first u) 'expt) (eql (first v) 'expt) (equalp (second u) (second v))) (update-terms (exponentiate `(,(second u) ,(add (list (third u) (third v))))) v i j terms))))))

; Handle an exponent
(defun exponentiate (terms)
        ;(format t "^Terms:~a~%" terms)
        (let ((base (first terms)) (power (second terms)))
                (cond
                        ; Power of 0
                        ((equalp power 0) 1)
                        ((and (equalp base 0) (numberp power) (< power 0)) (error "Division by zero"))
                        ; Base and power are numbers
                        ((and (numberp base) (numberp power)) (expt base power))
                        ; Nested exponents
                        ((and (listp base) (eql (first base) 'expt)) (exponentiate `(,(second base) ,(multiply (list (third base) power)))))
                        ; Power is a logarithm of same base
                        ((and (listp power) (eql (first power) 'log))
                         (cond
                                 ((and (eql (length power) 3) (equalp base (third power))) (second power))
                                 ((and (eql (length power) 2) (eql base 'e)) (second power))
                                 (t `(expt ,base ,power))))
                        (t `(expt ,base ,power)))))

; Handle logarithms
(defun logarithm (terms)
        ;(format t "logTerms:~a~%" terms)
        ; If no base, default to e
        (if (eql (length terms) 1) (setf terms (append terms '(e))))
        (let ((value (first terms)) (base (second terms)))
                (cond
                        ; Value of 1
                        ((equalp value 1) 0)
                        ; Value and base are the same
                        ((equalp value base) 1)
                        ; Base and value are numbers
                        ((and (numberp base) (numberp value)) (log base value))
                        ; Logarithm of an exponent
                        ((and (listp value) (eql (first value) 'expt)) (multiply `(,(third value) ,(logarithm (list (second value) base)))))
                        ; Otherwise, just return log. Drop base for base e
                        (t (if (eql base 'e) `(log ,value) `(log ,value ,base))))))

; Negate a list of terms, no operation included
(defun negate (terms)
        (let ((result (copy-tree terms)))
                (dotimes (x (length result))
                        (setf (nth x result) (multiply `(-1 ,(nth x result)))))
                result))

; Find the reciprocal of all terms in a list
(defun reciprocal (terms)
        (let ((result (copy-tree terms)))
                (dotimes (x (length result))
                        (setf (nth x result) (exponentiate `(,(nth x result) -1))))
                result))

;;; math.lisp ends here

