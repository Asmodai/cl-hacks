;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; color.lisp --- Functions for manipulating and dealing with colour values.
;;;
;;; Time-stamp: <Wednesday Oct 12, 2011 13:36:14 asmodai>
;;; Revision:   4
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

;;; ==================================================================
;;; {{{ HSV colour space:

;; The HSV colour space has three coordinates: hue, saturation, and
;; value (sometimes called brighness) respectively. This colour system is
;; attributed to "Smith" around 1978 and used to be called the hexcone
;; colour model. The hue is an angle from 0 to 360 degrees, typically 0
;; is red, 60 degrees yellow, 120 degrees green, 180 degrees cyan, 240
;; degrees blue, and 300 degrees magenta. Saturation typically ranges
;; from 0 to 1 (sometimes 0 to 100%) and defines how grey the colour is,
;; 0 indicates grey and 1 is the pure primary colour. Value is similar to
;; luninance except it also varies the colour saturation. If the colour
;; space is represented by disks of varying lightness then the hue and
;; saturation are the equivalent to polar coordinates (r,theta) of any
;; point in the plane. The disks on the right show this for various
;; values.

(defun hsv->rgb (h s v)
  "Convert a a colour value from HSV to RGB."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum h s v))
  (when (zerop s)
    (return-from hsv->rgb (values v v v)))
  (while (minusp h)
    (incf h 360))
  (while (>= h 360)
    (decf h 360))
  (let ((h-pos (/ h 60)))
    (multiple-value-bind (h-int h-frac) (truncate h-pos)
      (declare (fixnum h-int))
      (let ((p (* v (- 1 s)))
            (q (* v (- 1 (* s h-frac))))
            (t_ (* v (- 1 (* s (- 1 h-frac)))))
            r g b)
        (cond
  ((zerop h-int)
   (setf r v
 g t_
 b p))
  ((= 1 h-int)
   (setf r q
 g v
 b p))
  ((= 2 h-int)
   (setf r p
 g v
 b t_))
  ((= 3 h-int)
   (setf r p
 g q
 b v))
  ((= 4 h-int)
   (setf r t_
 g p
 b v))
  ((= 5 h-int)
   (setf r v
 g p
 b q)))
        (values r g b)))))

(defun hsv255->rgb255 (h s v)
  (declare (optimize (speed 3) (safety 0) (space 0)
                     (compilation-speed 0)))
  (when (zerop s)
    (return-from hsv255->rgb255 (values v v v)))
  (locally (declare (type fixnum h s v))
    (while (minusp h)
      (incf h 360))
    (while (>= h 360)
      (decf h 360))
    (let ((h-pos (/ h 60)))
      (multiple-value-bind (h-int h-frac) (truncate h-pos)
        (declare (fixnum h-int))
        (let* ((fs (/ s 255))
               (fv (/ v 255))
               (p (round (* 255 fv (- 1 fs))))
               (q (round (* 255 fv (- 1 (* fs h-frac)))))
               (t_ (round (* 255 fv (- 1 (* fs (- 1 h-frac))))))
               r g b)
          (cond
    ((zerop h-int)
     (setf r v
   g t_
   b p))
    ((= 1 h-int)
     (setf r q
   g v
   b p))
    ((= 2 h-int)
     (setf r p
   g v
   b t_))
    ((= 3 h-int)
     (setf r p
   g q
   b v))
    ((= 4 h-int)
     (setf r t_
   g p
   b v))
    ((= 5 h-int)
     (setf r v
   g p
   b q)))
          (values r g b))))))

(defun rgb->hsv (r g b)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((min (min r g b))
         (max (max r g b))
         (delta (- max min))
         (v max)
         (s 0)
         (h nil))
    (when (plusp max)
      (setq s (/ delta max)))
    (when (plusp delta)
      (setq h (* 60  (cond
                       ((= max r) (/ (- g b) delta))
                       ((= max g) (+ 2 (/ (- b r) delta)))
                       (t (+ 4 (/ (- r g) delta))))))
      (when (minusp h)
        (incf h 360)))
    (values h s v)))

(defun rgb255->hsv255 (r g b)
  "Integer convert from rgb from 0-255 -> h from 0-360 and sv from
0-255"
  (declare (fixnum r g b)
           (optimize (speed 3) (safety 0) (space 0)
                     (compilation-speed 0)))
  (let* ((min (min r g b))
         (max (max r g b))
         (delta (- max min))
         (v max)
         (s 0)
         (h nil))
    (declare (fixnum min max delta v s)
             (type (or null fixnum) h))
    (when (plusp max)
      (setq s (round (the fixnum (* 255 delta)) max)))
    (when (plusp delta)
      (setq h (cond
((= max r)
 (round (the fixnum (* 60 (the fixnum (- g b))))
                        delta))
((= max g)
 (the fixnum
   (+ 120
                      (round
                       (the fixnum (* 60
                                      (the fixnum (- b r)))) delta))))
(t
 (the fixnum
   (+ 240
                      (round
                       (the fixnum (* 60
                                      (the fixnum (- r g))))
                       delta))))))
      (when (minusp h)
        (incf h 360)))
    (values h s v)))

(defun hsv-equal (h1 s1 v1 h2 s2 v2 &key (limit .001))
  (declare (optimize (speed 3) (safety 0) (space 0)
                     (compilation-speed 0)))
  (flet ((~= (a b)
           (cond
     ((and (null a) (null b))
      t)
     ((or (null a) (null b))
      nil)
     (t
      (< (abs (- a b)) limit)))))
    (cond
      ((and (~= 0 v1) (~= 0 v2))
       t)
      ((or (null h1) (null h2))
       (when (and (~= 0 s1) (~= 0 s2) (~= v1 v2))
 t))
      (t
       (when (~= h1 h2) (~= s1 s2) (~= v1 v2)
     t)))))

(defun hsv255-equal (h1 s1 v1 h2 s2 v2 &key (limit 1))
  (declare (type fixnum s1 v1 s2 v2 limit)
           (type (or null fixnum) h1 h2)
           (optimize (speed 3) (safety 0) (space 0)
                     (compilation-speed 0)))
  (flet ((~= (a b)
           (declare (type (or null fixnum) a b))
           (cond
     ((and (null a) (null b))
      t)
     ((or (null a) (null b))
      nil)
     (t
      (<= (abs (the fixnum (- a b))) limit)))))
    (cond
      ((and (~= 0 v1) (~= 0 v2))
       t)
      ((or (null h1) (null h2))
       (when (and (~= 0 s1) (~= 0 s2) (~= v1 v2))
 t))
      (t
       (when (~= h1 h2) (~= s1 s2) (~= v1 v2)
     t)))))

(defun hsv-similar (h1 s1 v1 h2 s2 v2 &key
                    (hue-range 15) (value-range .2)
                    (saturation-range 0.2)
                    (gray-limit 0.3) (black-limit 0.3))
  "Returns T if two HSV values are similar."
  (cond
    ;; all black colors are similar
    ((and (<= v1 black-limit) (<= v2 black-limit))
     t)
    ;; all desaturated (gray) colors are similar for a value, despite
    ;; hue
    ((and (<= s1 gray-limit) (<= s2 gray-limit))
     (when (<= (abs (- v1 v2)) value-range)
       t))
    (t
     (when (and (<= (abs (hue-difference h1 h2)) hue-range)
(<= (abs (- v1 v2)) value-range)
(<= (abs (- s1 s2)) saturation-range))
       t))))

(defun hsv255-similar (h1 s1 v1 h2 s2 v2
                       &key (hue-range 15) (value-range 50)
                       (saturation-range 50)
                       (gray-limit 75) (black-limit 75))
  "Returns T if two HSV values are similar."
  (declare (fixnum s1 v1 s2 v2 hue-range value-range saturation-range
                   gray-limit black-limit)
           (type (or null fixnum) h1 h2)
           (optimize (speed 3) (safety 0) (space 0)
                     (compilation-speed 0)))
  (cond
    ;; all black colors are similar
    ((and (<= v1 black-limit) (<= v2 black-limit))
     t)
    ;; all desaturated (gray) colors are similar for a value, despite
    ;; hue
    ((and (<= s1 gray-limit) (<= s2 gray-limit))
     (when (<= (abs (- v1 v2)) value-range)
       t))
    (t
     (when (and (<= (abs (hue-difference-fixnum h1 h2)) hue-range)
(<= (abs (- v1 v2)) value-range)
(<= (abs (- s1 s2)) saturation-range))
       t))))

(defun hue-difference (h1 h2)
  "Return difference between two hues around 360 degree circle"
  (cond
    ((and (null h1) (null h2))
     t)
    ((or (null h1) (null h2))
     360)
    (t
     (let ((diff (- h2 h1)))
       (cond
 ((< diff -180)
  (+ 360 diff))
 ((> diff 180)
  (- (- 360 diff)))
 (t
  diff))))))

(defun hue-difference-fixnum (h1 h2)
  "Return difference between two hues around 360 degree circle"
  (cond
    ((and (null h1) (null h2))
     t)
    ((or (null h1) (null h2))
     360)
    (t
     (locally (declare (type fixnum h1 h2))
       (let ((diff (- h2 h1)))
 (cond
   ((< diff -180)
    (+ 360 diff))
   ((> diff 180)
    (- (- 360 diff)))
   (t
    diff)))))))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ RGB colour space:

(defun rgb->hex (r g b)
  "Convert an RGB colour value into a hexadecimal string."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum r g b))
  (format t "#~2,'0X~2,'0X~2,'0X" r g b))

(defun rgb-invert (r g b)
  "Invert an RGB colour value."
  (values (- 255 r)
          (- 255 g)
          (- 255 b)))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Colour class:

;;;
;;; Note that this will eventually replace the stuff above.
;;;
;;; For now, this is mostly undocumented, and will probably be very
;;; messy. So there.
;;;

(defclass color ()
  ((red-component
    :initform 0.0
    :initarg :red
    :accessor red-component)
   (blue-component
    :initform 0.0
    :initarg :blue
    :accessor blue-component)
   (green-component
    :initform 0.0
    :initarg :green
    :accessor green-component)
   (alpha-component
    :initform 1.0
    :initarg :alpha
    :accessor alpha-component)))

(defmethod print-object ((color color) stream)
  (print-unreadable-object (color stream :type t)
    (format stream "R=~'0D, G=~'0D, B=~'0D, A=~'0D"
            (red-component color)
            (green-component color)
            (blue-component color)
            (alpha-component color))))

(defmethod saturation-component ((color color))
  (let* ((r (red-component color))
         (g (green-component color))
         (b (blue-component color))
         (maxrgb (max r g b))
         (minrgb (min r g b))
         (delta (- maxrgb minrgb))
         (s (if (/= maxrgb 0.0)
                (/ delta maxrgb)
                0.0)))
    (multiple-value-bind (v d)
        (round (* s 100))
      (declare (ignore d))
      v)))

(defmethod hue-component ((color color))
  (let* ((r (red-component color))
         (g (green-component color))
         (b (blue-component color))
         (maxrgb (max r g b))
         (minrgb (min r g b))
         (delta (- maxrgb minrgb))
         (hue 0.0))
    (if (= (saturation-component color) 0.0)
        0.0
        (let* ((rr (/ (- maxrgb r) delta))
               (gr (/ (- maxrgb g) delta))
               (br (/ (- maxrgb b) delta)))
          (setf hue
                (/ (cond ((= r maxrgb) (- br gr))
                         ((= g maxrgb) (+ 2 (- rr br)))
                         (t (+ 4 (- gr rr))))
                   6.0))
          (if (< hue 0.0)
              (1+ hue))
          (multiple-value-bind (v d)
              (round (* hue 360))
            (declare (ignore d))
            v)))))
    
(defmethod brightness-component ((color color))
  (let* ((r (red-component color))
         (g (green-component color))
         (b (blue-component color))
         (maxrgb (max r g b))
         (minrgb (min r g b))
         (delta (- maxrgb minrgb)))
    (multiple-value-bind (v d)
        (round (* maxrgb 100))
      (declare (ignore d))
      v)))

(defmethod color->rgb255 ((color color))
  (values (* (red-component color) 255)
          (* (green-component color) 255)
          (* (blue-component color) 255)))

(defmethod color->rgba255 ((color color))
  (values (* (red-component color) 255)
          (* (green-component color) 255)
          (* (blue-component color) 255)
          (* (alpha-component color) 255)))

(defmethod color->hsb ((color color))
  (values (hue-component color)
          (saturation-component color)
          (brightness-component color)))

;;; }}}
;;; ==================================================================




;;; color.lisp ends here
