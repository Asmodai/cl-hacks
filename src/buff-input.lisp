;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; buff-input.lisp --- Buffered input.
;;;
;;; Time-stamp: <Monday Dec  5, 2011 05:06:22 asmodai>
;;; Revision:   6
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

#-genera
(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3)
                     (safety 0)
                     (space 0)
                     (debug 0))))

(defconstant +max-field+ 10000)
(defconstant +max-fields-per-line+ 20)
(defconstant +field-delim+ #\|)
(defconstant +eof-char+ #\rubout)
(defconstant +newline+ #\Newline)

(declaim (type character +eof-char+ +field-delim+ +newline+)
         (type fixnum +max-field+ +max-fields-per-line+))

;; Buffered fields parsing function
;; Uses fill-pointer for size

(defun make-fields-buffer (&optional
                           (max-fields +max-fields-per-line+)
                           (max-field-len +max-field+))
  (let ((bufs (make-array max-fields
                          :element-type 'vector
                          :fill-pointer 0
                          :adjustable nil)))
    (dotimes (i +max-fields-per-line+)
      (setf (aref bufs i) (make-array max-field-len
                                      :element-type 'character
                                      :fill-pointer 0
                                      :adjustable nil)))
    bufs))

(defun read-buffered-fields (fields strm &optional
                             (field-delim +field-delim+)
                             (eof 'eof))
  (declare (type base-char field-delim)
           (type vector fields))
  (setf (fill-pointer fields) 0)
  (do ((ifield 0 (1+ ifield))
       (linedone nil)
       (is-eof nil))
      (linedone (if is-eof eof fields))
    (declare (type fixnum ifield)
             (type boolean linedone is-eof))
    (let ((field (aref fields ifield)))
      (declare (type base-string field))
      (do ((ipos 0)
           (fielddone nil)
           (rc (read-char strm nil +eof-char+)
               (read-char strm nil +eof-char+)))
          (fielddone (unread-char rc strm))
        (declare (type fixnum ipos)
                 (type base-char rc)
                 (type boolean fielddone))
        (cond
          ((char= rc field-delim)
           (setf (fill-pointer field) ipos)
           (setq fielddone t))
          ((char= rc +newline+)
           (setf (fill-pointer field) ipos)
           (setf (fill-pointer fields) ifield)
           (setq fielddone t)
           (setq linedone t))
          ((char= rc +eof-char+)
           (setf (fill-pointer field) ipos)
           (setf (fill-pointer fields) ifield)
           (setq fielddone t)
           (setq linedone t)
           (setq is-eof t))
          (t
           (setf (char field ipos) rc)
           (incf ipos)))))))

;; Buffered fields parsing
;; Does not use fill-pointer
;; Returns 2 values -- string array and length array
(defstruct field-buffers
  (nfields 0 :type fixnum)
  (buffers)
  (field-lengths))

(defun make-fields-buffer2 (&optional
                            (max-fields +max-fields-per-line+)
                            (max-field-len +max-field+))
  (let ((bufs (make-array max-fields
                          :element-type 'vector 
                          :fill-pointer nil 
                          :adjustable nil))
        (bufstruct (make-field-buffers)))
    (dotimes (i +max-fields-per-line+)
      (setf (aref bufs i) (make-array max-field-len 
                                      :element-type 'character 
                                      :fill-pointer nil 
                                      :adjustable nil)))
    (setf (field-buffers-buffers bufstruct) bufs)
    (setf (field-buffers-field-lengths bufstruct)
          (make-array +max-fields-per-line+
                      :element-type 'fixnum 
                      :fill-pointer nil 
                      :adjustable nil))
    (setf (field-buffers-nfields bufstruct) 0)
    bufstruct))


(defun read-buffered-fields2 (fields strm &optional
                              (field-delim +field-delim+)
                              (eof 'eof))
  "Read a line from a stream into a field buffers"
  (declare (character field-delim))
  (setf (field-buffers-nfields fields) 0)
  (do ((ifield 0 (1+ ifield))
       (linedone nil)
       (is-eof nil))
      (linedone (if is-eof eof fields))
    (declare (fixnum ifield)
             (t linedone is-eof))
    (let ((field (aref (field-buffers-buffers fields) ifield)))
      (declare (simple-string field))
      (do ((ipos 0)
           (fielddone nil)
           (rc (read-char strm nil +eof-char+)
               (read-char strm nil +eof-char+)))
          (fielddone (unread-char rc strm))
        (declare (fixnum ipos)
                 (character rc)
                 (t fielddone))
        (cond
          ((char= rc field-delim)
           (setf (aref (field-buffers-field-lengths fields) ifield)
                 ipos)
           (setq fielddone t))
          ((char= rc +newline+)
           (setf (aref (field-buffers-field-lengths fields) ifield)
                 ipos)
           (setf (field-buffers-nfields fields) ifield)
           (setq fielddone t)
           (setq linedone t))
          ((char= rc +eof-char+)
           (setf (aref (field-buffers-field-lengths fields) ifield)
                 ipos)
           (setf (field-buffers-nfields fields) ifield)
           (setq fielddone t)
           (setq linedone t)
           (setq is-eof t))
          (t
           (setf (char field ipos) rc)
           (incf ipos)))))))

(defun bfield (fields i)
  (if (>= i (field-buffers-nfields fields))
      nil
      (subseq (aref (field-buffers-buffers fields) i)
              0
              (aref (field-buffers-field-lengths fields) i))))

;;; Buffered line parsing function

(defconstant +max-line+ 20000)
(let ((linebuffer (make-array +max-line+
                              :element-type 'character
                              :fill-pointer 0)))
  (defun read-buffered-line (strm eof)
    (declare (optimize (speed 3)
                       (space 0)
                       (safety 0)))
    (let ((pos 0)
          (done nil))
      (declare (fixnum pos) (type boolean done))
      (setf (fill-pointer linebuffer) 0)
      (do ((c (read-char strm nil +eof-char+)
              (read-char strm nil +eof-char+)))
          (done (progn
                  (unless (eql c +eof-char+) (unread-char c strm))
                  (if (eql c +eof-char+) eof linebuffer)))
        (declare (character c))
        (cond
          ((>= pos +max-line+)
           (warn "Line overflow")
           (setf done t))
          ((char= c #\Newline)
           (when (plusp pos)
             (setf (fill-pointer linebuffer) (1- pos)))
           (setf done t))
          ((char= +eof-char+)
           (setf done t))
          (t
           (setf (char linebuffer pos) c)
           (incf pos)))))))

;;; buff-input.lisp ends here
