;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; serialize.lisp --- Basic serialization support
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 00:37:01 asmodai>
;;; Revision:   11
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Based on `Rucksack' by Arthur Lemmens:
;;; Copyright (c) 2006  Arthur Lemmens.
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    15 Dec 2011 00:49:07
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

;;;==================================================================
;;;{{{ Constants:

(defconstant +illegal-marker+ 0
  "This should never be read as a marker.")

(defconstant +ignore+ 1
  "This marker is automatically skipped when read.")

;;;
;;; Booleans
(defconstant +nil+ 2)
(defconstant +t+ 3)

;;;
;;; Integers
(defconstant +minus-one+ #x09)
(defconstant +zero+ #x0A)
(defconstant +one+ #x0B)
(defconstant +two+ #x0C)

(defconstant +positive-byte-8+ #x10)
(defconstant +negative-byte-8+ #x11)
(defconstant +positive-byte-16+ #x12)
(defconstant +negative-byte-16+ #x13)
(defconstant +positive-byte-24+ #x14)
(defconstant +negative-byte-24+ #x15)
(defconstant +positive-byte-32+ #x16)
(defconstant +negative-byte-32+ #x17)
(defconstant +positive-byte-48+ #x18)
(defconstant +negative-byte-48+ #x19)
(defconstant +positive-byte-64+ #x1A)
(defconstant +negative-byte-64+ #x1B)
(defconstant +positive-integer+ #x1C)
(defconstant +negative-integer+ #x1D)

;;;
;;; Other numbers
(defconstant +rational+ #x20)
(defconstant +float+ #x21)
(defconstant +short-float+ #x22)
(defconstant +single-float+ #x23)
(defconstant +double-float+ #x24)
(defconstant +long-float+ #x25)
(defconstant +complex+ #x26)

;;;
;;; Strings and characters
(defconstant +character+ #x30)
(defconstant +character-8+ #x31)
(defconstant +character-16+ #x32)
(defconstant +character-24+ #x33)
(defconstant +character-32+ #x34)

(defconstant +base-char+ #x35)
(defconstant +extended-char+ #x36)

(defconstant +string+ #x40)
(defconstant +string-8+ #x41)
(defconstant +string-16+ #x42)
(defconstant +string-24+ #x43)
(defconstant +string-32+ #x44)
(defconstant +simple-string+ #x45)
(defconstant +simple-string-8+ #x46)
(defconstant +simple-string-16+ #x47)
(defconstant +simple-string-24+ #x48)
(defconstant +simple-string-32+ #x49)

;;;
;;; Symbols and packages
(defconstant +symbol+ #x50)
(defconstant +keyword+ #x51)
(defconstant +uninterned-symbol+ #x52)
(defconstant +symbol-reference+ #x53)
(defconstant +package+ #x54)

;;;
;;; Lists, conses, and structures
(defconstant +cons+ #x60)
(defconstant +proper-list+ #x61)
(defconstant +struct+ #x62)
(defconstant +struct-definition+ #x63)
(defconstant +dotted-list+ #x64)

;;;
;;; Objects and slots
(defconstant +object+ #x70)
(defconstant +unbound-slot+ #x71)
(defconstant +shared-object-definition+ #x72)
(defconstant +shared-object-reference+ #x73)
(defconstant +structure-object+ #x77)

;;;
;;; The Rest
(defconstant +hash-table+ #x80)
(defconstant +pathname+ #x90)
(defconstant +array+ #xA0)

;;;
;;; Garbage collector marks
(defconstant +free-block+ #xB0)
(defconstant +live-object+ #xB1)
(defconstant +dead-object+ #xB2)
(defconstant +reserved-object+ #xB3
  "Used for entries in the object table that belong to objects that haven't
been committed to disk yet.")

(defconstant +extension-0+ #xC0)

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Serializer class and base serialize/deserialize methods:

(defclass serializer ()
  ((stream
    :initarg :stream
    :reader serializer-stream
    :documentation "An (UNSIGNED-BYTE 8) stream.")))

;;;------------------------------------------------------------------
;;;{{{ Generic methods:

(defgeneric save-slots (object serializer)
  (:documentation "Save the slots of the given object to a stream."))

(defgeneric load-slots (object serializer)
  (:documentation "Load the slots of a given object from a stream."))

(defgeneric serialize-byte (byte serializer)
  (:documentation "Writes an unsigned-byte to a serializer.")
  (:method ((byte integer) (serializer serializer))
    (write-byte byte (serializer-stream serializer)))
  (:method ((byte integer) (stream stream))
    (write-byte byte stream)))

(defgeneric deserialize-byte (serializer &optional eof-error-p)
  (:documentation "Reads an unsigned-byte from a serializer.")
  (:method ((serializer serializer) &optional (eof-error-p t))
    (read-byte (serializer-stream serializer) eof-error-p nil))
  (:method ((stream stream) &optional (eof-error-p t))
    (read-byte stream eof-error-p nil)))

(defgeneric scan-byte (serializer)
  (:documentation "Skips an unsigned-byte from the serializer.")
  (:method ((serializer serializer))
    (read-byte (serializer-stream serializer) t nil))
  (:method ((stream stream))
    (read-byte stream t nil)))

(defgeneric serialize (object serializer)
  (:documentation "Writes a serialized version of an object to the
stream in a serializer."))

(defgeneric deserialize-contents (marker stream)
  (:method ((marker (eql +illegal-marker+)) stream)
    (cerror "Ignore the marker and continue."
            "There is an illegal marker in stream ~A."
            stream)))

(defgeneric scan-contents (marker serializer)
  (:method (marker serializer)
    (deserialize-contents marker serializer)))

;;;}}}
;;;------------------------------------------------------------------

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Scanner:

(defgeneric scan (buffer)
  (:documentation "Scans the object in the serialization buffer.")
  (:method ((buffer stream))
    (let ((marker (read-next-marker buffer)))
      (unless marker
        (cerror "Ignore the error and continue."
                "Could not find the next scan marker.")
        (return-from scan))
      (scan-contents marker buffer)))
  (:method ((buffer serializer))
    (scan (serializer-stream buffer))))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Serialize/deserialize/scan functions:

;;;------------------------------------------------------------------
;;;{{{ Common:

(defun serialize-marker (marker serializer)
  (serialize-byte marker serializer))

(defun read-next-marker (serializer)
  "Returns the next marker (or NIL if we're at the end of the
serializer stream."
  (loop (let ((marker (deserialize-byte serializer nil)))
          (if (null marker)
              (return nil)
              (unless (eql marker +ignore+)
                (return marker))))))

(defun deserialize (serializer
                    &optional (eof-error-p t) (eof-value nil))
  "Reads the next object from the serializer stream.  If EOF-ERROR-P
is non-NIL, signals an end-of-file error or returns EOF-VALUE when the
end of the stream is reached.  EOF-ERROR-P defaults to T."
  (let ((marker (read-next-marker serializer)))
    (if marker
        (deserialize-contents marker serializer)
        (if eof-error-p
            (error 'end-of-file :stream serializer)
            eof-value))))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Lists:

(defun serialize-list (list stream &optional (length (length list)))
  "Serializes a proper list by first serializing its length and then
all of the elements of the list."
  (serialize length stream)
  (dolist (elt list)
    (serialize elt stream)))

(defun deserialize-list (stream)
  "Deserializes a list by deserializing its length and then all of the
elements of the list."
  (let ((length (deserialize stream)))
    (loop repeat length
          collect (deserialize stream))))

(defun serialize-dotted-list (list stream
                              &optional (length (length list)))
  "Serializes a dotted list by first serializing its length and then
all of the elements of the list."
  (serialize length stream)
  (loop for elt on list
        do (serialize (car elt) stream)
           (when (atom (cdr elt))
             (serialize (cdr elt) stream))))

(defun deserialize-dotted-list (stream)
  "Deserializes a dotted list by first deserializing its length and
then all of the elements of the list."
  (let* ((length (deserialize stream))
         (list (loop repeat (1- length)
                     collect (deserialize stream)))
         (final-elt (deserialize stream)))
    (setf (cdr (last list)) final-elt)
    list))

(defun analyze-list (x)
  "Returns two values: the first value is one of :PROPER-LIST,
:DOTTED-LIST, OR :CIRCULAR-LIST.  The second value is the length of the
  list.  For dotted lists, the final item is included in the length; for
  circular lists, the length is NIL."
  (do ((n 0 (+ n 2))
       (fast x (cddr fast))
       (slow x (cdr slow)))
      (nil)
    (cond ((null fast)
           (return (values :proper-list n)))
          ((atom fast)
           (return (values :dotted-list (1+ n))))
          ((null (cdr fast))
           (return (values :proper-list (1+ n))))
          ((atom (cdr fast))
           (return (values :dotted-list (+ n 2))))
          ((and (eq fast slow)
                (> n 0))
           (return (values :circular-list nil))))))

(defun scan-list (stream)
  (let ((length (deserialize stream)))
    (loop repeat length
          do (scan stream))))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Integers:

(defun nr-octets (n)
  (ceiling (integer-length n) 8))

(defun serialize-byte-16 (integer stream)
  (serialize-byte (ldb (byte 8 0) integer) stream)
  (serialize-byte (ldb (byte 8 8) integer) stream))

(defun serialize-byte-24 (integer stream)
  (serialize-byte (ldb (byte 8 0) integer) stream)
  (serialize-byte (ldb (byte 8 8) integer) stream)
  (serialize-byte (ldb (byte 8 16) integer) stream))

(defun serialize-byte-32 (integer stream)
  (serialize-byte (ldb (byte 8 0) integer) stream)
  (serialize-byte (ldb (byte 8 8) integer) stream)
  (serialize-byte (ldb (byte 8 16) integer) stream)
  (serialize-byte (ldb (byte 8 24) integer) stream))

(defun serialize-byte-48 (integer stream)
  (multiple-value-bind (msb lsb)
      (floor integer #x1000000)
    (serialize-byte-24 lsb stream)
    (serialize-byte-24 msb stream)))

(defun serialize-byte-64 (integer stream)
  (multiple-value-bind (msb lsb)
      (floor integer #x100000000)
    (serialize-byte-32 lsb stream)
    (serialize-byte-32 msb stream)))

(defun deserialize-byte-16 (stream)
  (+ (deserialize-byte stream)
     (* (deserialize-byte stream) 256)))

(defun deserialize-byte-24 (stream)
  (+ (deserialize-byte stream)
     (* (deserialize-byte stream) #x100)
     (* (deserialize-byte stream) #x10000)))

(defun deserialize-byte-32 (stream)
  (+ (deserialize-byte stream)
     (* (deserialize-byte stream) #x100)
     (* (deserialize-byte stream) #x10000)
     (* (deserialize-byte stream) #x1000000)))

(defun deserialize-byte-48 (stream)
  (+ (deserialize-byte-24 stream)
     (* (deserialize-byte-24 stream) #x1000000)))

(defun deserialize-byte-64 (stream)
  (+ (deserialize-byte-32 stream)
     (* (deserialize-byte-32 stream) #x100000000)))

(defun scan-byte-16 (stream)
  (scan-byte stream)
  (scan-byte stream))

(defun scan-byte-24 (stream)
  (dotimes (i 3)
    (scan-byte stream)))

(defun scan-byte-32 (stream)
  (scan-byte-16 stream)
  (scan-byte-16 stream))

(defun scan-byte-48 (stream)
  (scan-byte-24 stream)
  (scan-byte-24 stream))

(defun scan-byte-64 (stream)
  (scan-byte-32 stream)
  (scan-byte-32 stream))

(defun serialize-integer (positive-p unsigned nr-octets stream)
  (case nr-octets
    (1
     (serialize-marker (if positive-p
                           +positive-byte-8+
                           +negative-byte-8+)
                       stream)
     (serialize-byte unsigned stream))
    (2
     (serialize-marker (if positive-p
                           +positive-byte-16+
                           +negative-byte-16+)
                       stream)
     (serialize-byte-16 unsigned stream))
    (3
     (serialize-marker (if positive-p
                           +positive-byte-24+
                           +negative-byte-24+)
                       stream)
     (serialize-byte-24 unsigned stream))
    (4
     (serialize-marker (if positive-p
                           +positive-byte-32+
                           +negative-byte-32+)
                       stream)
     (serialize-byte-32 unsigned stream))
    ((5 6)
     (serialize-marker (if positive-p
                           +positive-byte-48+
                           +negative-byte-48+)
                       stream)
     (serialize-byte-48 unsigned stream))
    ((7 8)
     (serialize-marker (if positive-p
                           +positive-byte-64+
                           +negative-byte-64+)
                       stream)
     (serialize-byte-64 unsigned stream))
    (otherwise
     (let ((nr-bits (* 8 nr-octets)))
       (serialize-marker (if positive-p
                             +positive-integer+
                             +negative-integer+)
                         stream)
       (serialize nr-octets stream)
       (loop for position from (- nr-bits 8) downto 0 by 8
             do (serialize-byte (ldb (byte 8 position) unsigned)
                                stream))))))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Floats:

(defun serialize-float (number stream)
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float number)
    (serialize significand stream)
    (serialize exponent stream)
    (serialize sign stream)))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Characters and strings:

(defun max-character-code (string)
  "Returns the highest character code in the given string."
  (loop for char across string
        maximize (char-code char)))

(defun scan-string (simple-p scanner stream)
  (scan-byte stream)                    ; Skip the type marker.
  (unless simple-p
    (scan stream)                       ; Fill pointer.
    (scan stream))                      ; Adjustable.
  (loop repeat (deserialize stream)     ; Length.
        do (funcall scanner stream)))

(defun deserialize-string (simple-p reader stream)
  (let* ((type-marker (deserialize-byte stream))
         (type 'character)
         (fill-pointer (unless simple-p
                         (deserialize stream)))
         (adjustable-p (unless simple-p
                         (deserialize stream)))
         (length (deserialize stream)))
    (declare (ignore type-marker))
    (let ((string (if simple-p
                      (make-string length
                                   :element-type type)
                      (make-array length
                                  :element-type type
                                  :fill-pointer fill-pointer
                                  :adjustable adjustable-p))))
      (loop for i below length
            for code = (funcall reader stream)
            do (setf (char string i) (code-char code)))
      string)))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Objects:

(defun serialize-standard-object (object serializer)
  (serialize-marker +object+ serializer)
  (serialize (class-name (class-of object)) serializer)
  (save-slots object serializer))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Structures:

(defun serialize-structure-object (object stream)
  (serialize-marker +structure-object+ stream)
  (serialize (class-name (class-of object)) stream)
  (save-slots object stream))

;;;}}}
;;;------------------------------------------------------------------

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Serialize/deserialize methods:

;;;------------------------------------------------------------------
;;;{{{ Common:

(defmethod serialize ((object (eql nil)) stream)
  (serialize-marker +nil+ stream))

(defmethod serialize ((object (eql t)) stream)
  (serialize-marker +t+ stream))

(defmethod deserialize-contents ((marker (eql +nil+)) stream)
  (declare (ignore stream))
  nil)

(defmethod deserialize-contents ((marker (eql +t+)) stream)
  (declare (ignore stream))
  t)

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Integers:

(defmethod serialize ((obj integer) stream)
  (cond ((zerop obj)
         (serialize-marker +zero+ stream))
        ((= obj 1)
         (serialize-marker +one+ stream))
        ((= obj -1)
         (serialize-marker +minus-one+ stream))
        ((= obj 2)
         (serialize-marker +two+ stream))
        (t
         (let* ((positive-p (>= obj 0))
                (unsigned (abs obj))
                (nr-octets (nr-octets unsigned)))
           (serialize-integer positive-p
                              unsigned
                              nr-octets
                              stream)))))

(defmethod scan-contents ((marker (eql +positive-byte-8+)) stream)
  (scan-byte stream))

(defmethod scan-contents ((marker (eql +negative-byte-8+)) stream)
  (scan-byte stream))

(defmethod scan-contents ((marker (eql +positive-byte-16+)) stream)
  (scan-byte-16 stream))

(defmethod scan-contents ((marker (eql +negative-byte-16+)) stream)
  (scan-byte-16 stream))

(defmethod scan-contents ((marker (eql +positive-byte-24+)) stream)
  (scan-byte-24 stream))

(defmethod scan-contents ((marker (eql +negative-byte-24+)) stream)
  (scan-byte-24 stream))

(defmethod scan-contents ((marker (eql +positive-byte-32+)) stream)
  (scan-byte-32 stream))

(defmethod scan-contents ((marker (eql +negative-byte-32+)) stream)
  (scan-byte-32 stream))

(defmethod scan-contents ((marker (eql +positive-byte-48+)) stream)
  (scan-byte-48 stream))

(defmethod scan-contents ((marker (eql +negative-byte-48+)) stream)
  (scan-byte-48 stream))

(defmethod scan-contents ((marker (eql +positive-byte-64+)) stream)
  (scan-byte-64 stream))

(defmethod scan-contents ((marker (eql +negative-byte-64+)) stream)
  (scan-byte-64 stream))

(defmethod scan-contents ((marker (eql +positive-integer+)) stream)
  (let ((nr-bytes (deserialize stream)))
    (assert (integerp nr-bytes))
    (dotimes (i nr-bytes)
      (scan-byte stream))))

(defmethod scan-contents ((marker (eql +negative-integer+)) stream)
  (scan-contents +positive-integer+ stream))

(defmethod deserialize-contents ((marker (eql +minus-one+)) stream)
  (declare (ignore stream))
  -1)

(defmethod deserialize-contents ((marker (eql +zero+)) stream)
  (declare (ignore stream))
  0)

(defmethod deserialize-contents ((marker (eql +one+)) stream)
  (declare (ignore stream))
  1)

(defmethod deserialize-contents ((marker (eql +two+)) stream)
  (declare (ignore stream))
  2)

(defmethod deserialize-contents ((marker (eql +positive-byte-8+)) stream)
  (deserialize-byte stream))

(defmethod deserialize-contents ((marker (eql +negative-byte-8+)) stream)
  (deserialize-byte stream))

(defmethod deserialize-contents ((marker (eql +positive-byte-16+)) stream)
  (deserialize-byte-16 stream))

(defmethod deserialize-contents ((marker (eql +negative-byte-16+)) stream)
  (deserialize-byte-16 stream))

(defmethod deserialize-contents ((marker (eql +positive-byte-24+)) stream)
  (deserialize-byte-24 stream))

(defmethod deserialize-contents ((marker (eql +negative-byte-24+)) stream)
  (deserialize-byte-24 stream))

(defmethod deserialize-contents ((marker (eql +positive-byte-32+)) stream)
  (deserialize-byte-32 stream))

(defmethod deserialize-contents ((marker (eql +negative-byte-32+)) stream)
  (deserialize-byte-32 stream))

(defmethod deserialize-contents ((marker (eql +positive-byte-48+)) stream)
  (deserialize-byte-48 stream))

(defmethod deserialize-contents ((marker (eql +negative-byte-48+)) stream)
  (deserialize-byte-48 stream))

(defmethod deserialize-contents ((marker (eql +positive-byte-64+)) stream)
  (deserialize-byte-64 stream))

(defmethod deserialize-contents ((marker (eql +negative-byte-64+)) stream)
  (deserialize-byte-64 stream))

(defmethod deserialize-contents ((marker (eql +positive-integer+)) stream)
  (let ((nr-bytes (deserialize stream)))
    (assert (integerp nr-bytes))
    (let ((result 0))
      (loop for i below nr-bytes
            do (setf result (+ (ash result 8) (deserialize-byte stream))))
      result)))

(defmethod deserialize-contents ((marker (eql +negative-integer+)) stream)
  (- (deserialize-contents +positive-integer+ stream)))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Rationals:

(defmethod serialize ((number rational) stream)
  (serialize-marker +rational+ stream)
  (serialize (numerator number) stream)
  (serialize (denominator number) stream))

(defmethod deserialize-contents ((marker (eql +rational+)) stream)
  (/ (deserialize stream) (deserialize stream)))

(defmethod scan-contents ((marker (eql +rational+)) stream)
  (scan stream)
  (scan stream))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Floats:

(defmethod serialize ((number float) stream)
  (serialize-marker +float+ stream)
  (serialize-float number stream))

(defmethod deserialize-contents ((marker (eql +float+)) stream)
  (let* ((significand (deserialize stream))
         (exponent (deserialize stream))
         (sign (deserialize stream)))
    (* sign (scale-float (float significand 1.0L0) exponent))))

(defmethod scan-contents ((marker (eql +float+)) stream)
  (dotimes (i 3)
    (scan stream)))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Complex numbers:

(defmethod serialize ((number complex) stream)
  (serialize-marker +complex+ stream)
  (serialize (realpart number) stream)
  (serialize (imagpart number) stream))

(defmethod deserialize-contents ((marker (eql +complex+)) stream)
  (complex (deserialize stream) (deserialize stream)))

(defmethod scan-contents ((marker (eql +complex+)) stream)
  (scan stream)
  (scan stream))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Lists and conses:

(defmethod serialize ((cons cons) stream)
  (multiple-value-bind (list-type list-length)
      (analyze-list cons)
    (ecase list-type
      (:proper-list
       (serialize-marker +proper-list+ stream)
       (serialize-list cons stream list-length))
      (:dotted-list
       (serialize-marker +dotted-list+ stream)
       (serialize-dotted-list cons stream list-length))
      (:circular-list
       (error "Serialization of circular lists isn't supported yet.")))))

(defmethod deserialize-contents ((marker (eql +proper-list+)) stream)
  (deserialize-list stream))

(defmethod deserialize-contents ((marker (eql +dotted-list+)) stream)
  (deserialize-dotted-list stream))

(defmethod deserialize-contents ((marker (eql +cons+)) stream)
  (cons (deserialize stream) (deserialize stream)))

(defmethod scan-contents ((marker (eql +proper-list+)) stream)
  (scan-list stream))

(defmethod scan-contents ((marker (eql +dotted-list+)) stream)
  (scan-list stream))

(defmethod scan-contents ((marker (eql +cons+)) stream)
  (scan stream)
  (scan stream))


;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Strings and characters:

(defmethod serialize ((char character) stream)
  (unless (= (char-code char) (char-int char))
    (cerror "Serialize it anyway (without attributes)."
            "The character ~S cannot be serialized because it has
implementation-dependant attributes."
            char))
  (let ((code (char-code char)))
    (cond ((<= code #xFF)
           (serialize-marker +character-8+ stream)
           (serialize-byte code stream))
          ((<= code #xFFFF)
           (serialize-marker +character-16+ stream)
           (serialize-byte-16 code stream))
          ((<= code #xFFFFFF)
           (serialize-marker +character-24+ stream)
           (serialize-byte-24 code stream))
          ((<= code #xFFFFFFFF)
           (serialize-marker +character-32+ stream)
           (serialize-byte-32 code stream))
          (t
           (serialize-marker +character+ stream)
           (serialize code stream)))))

(defmethod deserialize-contents ((marker (eql +character+)) stream)
  (code-char (deserialize stream)))

(defmethod deserialize-contents ((marker (eql +character-8+)) stream)
  (code-char (deserialize-byte stream)))

(defmethod deserialize-contents ((marker (eql +character-16+)) stream)
  (code-char (deserialize-byte-16 stream)))

(defmethod deserialize-contents ((marker (eql +character-24+)) stream)
  (code-char (deserialize-byte-24 stream)))

(defmethod deserialize-contents ((marker (eql +character-32+)) stream)
  (code-char (deserialize-byte-32 stream)))

(defmethod scan-contents ((marker (eql +character+)) stream)
  (scan stream))

(defmethod scan-contents ((marker (eql +character-8+)) stream)
  (scan-byte stream))

(defmethod scan-contents ((marker (eql +character-16+)) stream)
  (scan-byte-16 stream))

(defmethod scan-contents ((marker (eql +character-24+)) stream)
  (scan-byte-24 stream))

(defmethod scan-contents ((marker (eql +character-32+)) stream)
  (scan-byte-32 stream))

(defmethod serialize ((string string) stream)
  (let ((max-code (max-character-code string))
        (simple-p (simple-string-p string)))
    (multiple-value-bind (marker writer)
        (cond ((<= max-code #xFF)
               (values (if simple-p
                           +simple-string-8+
                           +string-8+)
                       #'serialize-byte))
              ((<= max-code #xFFFF)
               (values (if simple-p
                           +simple-string-16+
                           +string-16+)
                       #'serialize-byte-16))
              ((<= max-code #xFFFFFF)
               (values (if simple-p
                           +simple-string-24+
                           +string-24+)
                       #'serialize-byte-24))
              ((<= max-code #xFFFFFFFF)
               (values (if simple-p
                           +simple-string-32+
                           +string-32+)
                       #'serialize-byte-32))
              (t
               (values (if simple-p
                           +simple-string+
                           +string+)
                       #'serialize)))
      (serialize-marker marker stream)
      (let* ((type (array-element-type string))
             (type-marker
              (cond ((subtypep type 'base-char)
                     +base-char+)
                    ((subtypep type 'extended-char)
                     +extended-char+)
                    ((subtypep type 'character)
                     +character+)
                    (t
                     (error "Unrecognized element-type ~S for string."
                            type)))))
        (serialize-marker type-marker stream))
      (unless simple-p
        (serialize (fill-pointer string) stream)
        (serialize (adjustable-array-p string) stream))
      (serialize (length string) stream)
      (loop for char across string
            for code = (char-code char)
            do (funcall writer code stream)))))

(defmethod scan-contents ((marker (eql +simple-string+)) stream)
  (scan-string t #'scan stream))

(defmethod scan-contents ((marker (eql +simple-string-8+)) stream)
  (scan-string t #'scan-byte stream))

(defmethod scan-contents ((marker (eql +simple-string-16+)) stream)
  (scan-string t #'scan-byte-16 stream))

(defmethod scan-contents ((marker (eql +simple-string-24+)) stream)
  (scan-string t #'scan-byte-24 stream))

(defmethod scan-contents ((marker (eql +simple-string-32+)) stream)
  (scan-string t #'scan-byte-32 stream))

(defmethod scan-contents ((marker (eql +string+)) stream)
  (scan-string nil #'scan stream))

(defmethod scan-contents ((marker (eql +string-8+)) stream)
  (scan-string nil #'scan-byte stream))

(defmethod scan-contents ((marker (eql +string-16+)) stream)
  (scan-string nil #'scan-byte-16 stream))

(defmethod scan-contents ((marker (eql +string-24+)) stream)
  (scan-string nil #'scan-byte-24 stream))

(defmethod scan-contents ((marker (eql +string-32+)) stream)
  (scan-string nil #'scan-byte-32 stream))

(defmethod deserialize-contents ((marker (eql +simple-string+)) stream)
  (deserialize-string t #'deserialize stream))

(defmethod deserialize-contents ((marker (eql +simple-string-8+)) stream)
  (deserialize-string t #'deserialize-byte stream))

(defmethod deserialize-contents ((marker (eql +simple-string-16+)) stream)
  (deserialize-string t #'deserialize-byte-16 stream))

(defmethod deserialize-contents ((marker (eql +simple-string-24+)) stream)
  (deserialize-string t #'deserialize-byte-24 stream))

(defmethod deserialize-contents ((marker (eql +simple-string-32+)) stream)
  (deserialize-string t #'deserialize-byte-32 stream))

(defmethod deserialize-contents ((marker (eql +string+)) stream)
  (deserialize-string nil #'deserialize stream))

(defmethod deserialize-contents ((marker (eql +string-8+)) stream)
  (deserialize-string nil #'deserialize-byte stream))

(defmethod deserialize-contents ((marker (eql +string-16+)) stream)
  (deserialize-string nil #'deserialize-byte-16 stream))

(defmethod deserialize-contents ((marker (eql +string-24+)) stream)
  (deserialize-string nil #'deserialize-byte-24 stream))

(defmethod deserialize-contents ((marker (eql +string-32+)) stream)
  (deserialize-string nil #'deserialize-byte-32 stream))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Symbols and packages:

(defmethod serialize ((symbol symbol) stream)
  (cond ((keywordp symbol)
         (serialize-marker +keyword+ stream)
         (serialize (symbol-name symbol) stream))
        ((null (symbol-package symbol))
         (serialize-marker +uninterned-symbol+ stream)
         (serialize (symbol-name symbol) stream))
        (t
         (serialize-marker +symbol+ stream)
         (serialize (package-name (symbol-package symbol)) stream)
         (serialize (symbol-name symbol) stream))))

(defmethod deserialize-contents ((marker (eql +keyword+)) stream)
  (intern (deserialize stream) (find-package :keyword)))

(defmethod scan-contents ((marker (eql +keyword+)) stream)
  (scan stream))

(defmethod deserialize-contents ((marker (eql +uninterned-symbol+)) stream)
  (make-symbol (deserialize stream)))

(defmethod scan-contents ((marker (eql +uninterned-symbol+)) stream)
  (scan stream))

(defmethod deserialize-contents ((marker (eql +symbol+)) stream)
  (let ((package-name (deserialize stream))
        (symbol-name (deserialize stream)))
    (let ((package (or (find-package package-name)
                       (cerror "Create the package and continue."
                               "Cannot find a package named ~S to intern
the symbol ~S."
                               package-name
                               symbol-name)
                       (make-package package-name))))
      (intern symbol-name package))))

(defmethod scan-contents ((marker (eql +symbol+)) stream)
  (scan stream)
  (scan stream))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Objects:

(defmethod saved-slots (object)
  "Use the MOP to return a list of the names of all effective slots."
  (mapcar #'slot-definition-name (class-slots (class-of object))))

(defmethod serialize ((object standard-object) serializer)
  (serialize-standard-object object serializer))

(defmethod save-slots ((object standard-object) serializer)
  (let ((slots (saved-slots object)))
    (serialize (length slots) serializer)
    (loop for slot-name in slots
          do (if (slot-boundp object slot-name)
                 (serialize (slot-value object slot-name) serializer)
                 (serialize-marker +unbound-slot+ serializer)))))

(defmethod deserialize-contents ((marker (eql +object+)) stream)
  (let* ((class-name (deserialize stream))
         (object (allocate-instance (find-class class-name))))
    (load-slots object stream)))

(defmethod load-slots ((object standard-object) stream)
  (let ((nr-slots (deserialize stream))
        (slots (saved-slots object)))
    (unless (= nr-slots (length slots))
      (error "Slot mismatch when deserialising a standard object of class ~S."
             (class-of object)))
    (loop for slot-name in slots
          do (let ((marker (read-next-marker stream)))
               (if (eql marker +unbound-slot+)
                   (slot-makunbound object slot-name)
                   (setf (slot-value object slot-name)
                         (deserialize-contents marker stream)))))
    object))

(defmethod scan-contents ((marker (eql +object+)) stream)
  (scan stream)
  (let ((nr-slots (deserialize stream)))
    (loop repeat nr-slots
          do (scan stream))))

(defmethod scan-contents ((marker (eql +unbound-slot+)) stream)
  :do-nothing)

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Structures:

(defmethod serialize ((object structure-object) stream)
  (serialize-structure-object object stream))

(defmethod save-slots ((object structure-object) stream)
  (let ((slots (saved-slots object)))
    (serialize (length slots) stream)
    (loop for slot-name in slots
          do (serialize (slot-value object slot-name) stream))))

(defmethod deserialize-contents ((marker (eql +structure-object+)) stream)
  (let* ((class-name (deserialize stream))
         (object (allocate-instance (find-class class-name))))
    (load-slots object stream)))

(defmethod load-slots ((object structure-object) stream)
  (let ((nr-slots (deserialize stream))
        (slots (saved-slots object)))
    (unless (= nr-slots (length slots))
      (error "Slot mismatch while deserializing a structure of class ~S."
             (class-of object)))
    (loop for slot-name in slots
          do (let ((marker (read-next-marker stream)))
               (setf (slot-value object slot-name)
                     (deserialize-contents marker stream))))
    object))

(defmethod scan-contents ((marker (eql +structure-object+)) stream)
  (scan stream)
  (let ((nr-slots (deserialize stream)))
    (loop repeat nr-slots
          do (scan stream))))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Arrays:

(defmethod serialize ((array array) stream)
  (serialize-marker +array+ stream)
  (serialize (array-element-type array) stream)
  (serialize-list (array-dimensions array) stream)
  (when (= 1 (array-rank array))
    (serialize (array-has-fill-pointer-p array) stream)
    (when (array-has-fill-pointer-p array)
      (serialize (fill-pointer array) stream)))
  (serialize (adjustable-array-p array) stream)
  (if (array-displacement array)
      (error "Saving displaced arrays is not supported yet.")
      (serialize nil stream))
  (loop for i below (array-total-size array)
        do (serialize (row-major-aref array i) stream)))

(defmethod deserialize-contents ((marker (eql +array+)) stream)
  (let* ((type (deserialize stream))
         (dimensions (deserialize-list stream))
         (has-fill-pointer-p nil)
         (fill-pointer nil)
         adjustable-p)
    (when (= 1 (length dimensions))
      (setq has-fill-pointer-p (deserialize stream))
      (when has-fill-pointer-p
        (setq fill-pointer (deserialize stream))))
    (setq adjustable-p (deserialize stream))
    (deserialize stream)                ; Skip displacement.
    (let ((array (make-array dimensions
                             :element-type type
                             :adjustable adjustable-p
                             :fill-pointer fill-pointer)))
      (loop for i below (array-total-size array)
            do (setf (row-major-aref array i)
                     (deserialize stream)))
      array)))

(defmethod scan-contents ((marker (eql +array+)) stream)
  (scan stream)                         ; scan type.
  (let ((dimensions (deserialize-list stream)))
    (when (= 1 (length dimensions))
      (let ((has-fill-pointer-p (deserialize stream)))
        (when has-fill-pointer-p
          (deserialize stream))))
    (deserialize stream)
    (deserialize stream)
    (let ((total-size (reduce #'+ dimensions)))
      (loop repeat total-size
            do (scan stream)))))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Pathnames:

(defmethod serialize ((pathname pathname) stream)
  (serialize-marker +pathname+ stream)
  (serialize (host-namestring pathname) stream)
  (serialize (pathname-device pathname) stream)
  (serialize (pathname-directory pathname) stream)
  (serialize (pathname-name pathname) stream)
  (serialize (pathname-type pathname) stream)
  (serialize (pathname-version pathname) stream))

(defmethod deserialize-contents ((marker (eql +pathname+)) stream)
  (let* ((host (deserialize stream))
         (device (deserialize stream))
         (directory (deserialize stream))
         (name (deserialize stream))
         (type (deserialize stream))
         (version (deserialize stream)))
    (make-pathname :host host
                   :device device
                   :directory directory
                   :name name
                   :type type
                   :version version)))

(defmethod scan-contents ((marker (eql +pathname+)) stream)
  (dotimes (i 6)
    (scan stream)))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Hash tables:

(defmethod serialize ((hash-table hash-table) stream)
  (serialize-marker +hash-table+ stream)
  (serialize (hash-table-test hash-table) stream)
  (serialize (hash-table-size hash-table) stream)
  (serialize (hash-table-rehash-size hash-table) stream)
  (serialize (hash-table-rehash-threshold hash-table) stream)
  (serialize (hash-table-count hash-table) stream)
  (maphash (lambda (k v)
             (serialize k stream)
             (serialize v stream))
           hash-table))

(defmethod deserialize-contents ((marker (eql +hash-table+)) stream)
  (let* ((test (deserialize stream))
         (size (deserialize stream))
         (rehash-size (deserialize stream))
         (rehash-threshold (deserialize stream))
         (count (deserialize stream)))
    (let ((table (make-hash-table :test test
                                  :size size
                                  :rehash-size rehash-size
                                  :rehash-threshold rehash-threshold)))
      (loop repeat count
            do (let* ((key (deserialize stream))
                      (value (deserialize stream)))
                 (setf (gethash key table) value)))
      table)))

(defmethod scan-contents ((marker (eql +hash-table+)) stream)
  (loop repeat 4                        ; Skip over dimensions
        do (scan stream))
  (let ((count (deserialize stream)))
    (loop repeat count
          do (scan stream)              ; Key
             (scan stream))))           ; Value

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Free list integration

(defmethod deserialize-contents ((marker (eql +free-block+)) stream)
  (declare (ignore stream))
  :free-block)

(defmethod deserialize-contents ((marker (eql +live-object+)) stream)
  (declare (ignore stream))
  :live-object)

(defmethod deserialize-contents ((marker (eql +reserved-object+)) stream)
  (declare (ignore stream))
  :reserved)

;;;}}}
;;;------------------------------------------------------------------

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Main functions:

(defun save-objects (objects filespec
                     &key (supersede nil) (error-if-not-exist nil))
  "Saves a list with OBJECTS to a file named by FILESPEC.  The file is
created if necessary, and will be superseded if it already exists."
  (ensure-directories-exist filespec)
  (with-open-file (stream filespec
                          :element-type '(unsigned-byte 8)
                          :direction :output
                          :if-exists (if supersede
                                         :supersede
                                         :append)
                          :if-does-not-exist (if error-if-not-exist
                                                 :error
                                                 :create))
    (let ((serializer (make-instance 'serializer
                         :stream stream)))
      (serialize-list objects serializer))))

(defun load-objects (filespec)
  "Returns a list of objects from the file given in FILESPEC."
  (with-open-file (stream filespec
                          :element-type '(unsigned-byte 8)
                          :direction :input)
    (let ((serializer (make-instance 'serializer
                         :stream stream)))
      (deserialize-list serializer))))

(defun open-serializer (stream)
  "Creates and returns a serializer for a stream.  The stream must
have an element-type of (UNSIGNED-BYTE 8)."
  (make-instance 'serializer :stream stream))

(defun close-serializer (serializer &key abort)
  (close (serializer-stream serializer) :abort abort))

(defun force-serializer-output (serializer)
  (force-output (serializer-stream serializer)))

;;;}}}
;;;==================================================================

;;; serialize.lisp ends here
