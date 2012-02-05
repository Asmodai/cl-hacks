;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; io.lisp --- I/O functions
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:44:05 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 22:39:30
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

;;; ------------------------------------------------------------------
;;;{{{ Files and streams:

(defun print-file-contents (file &optional (stream *standard-output*))
  (when (probe-file file)
    (let ((eof (cons 'eof nil)))
      (with-open-file (in file :direction :input)
        (do ((line (read-line in nil eof)
                   (read-line in nil eof)))
            ((eq line eof))
          (write-string line stream)
          (write-char #\Newline stream))))))

(defun read-stream-to-string (in)
  (with-output-to-string (out)
    (let ((eof (gensym)))
      (do ((line (read-line in nil eof)
                 (read-line in nil eof)))
          ((eq line eof))
        (format out "~A~%" line)))))

(defun read-file-to-string (file)
  (with-open-file (in file :direction :input)
    (read-stream-to-string in)))

(defun read-file-to-usb8-array (file)
  (with-open-file (in file :direction :input
                      :element-type '(unsigned-byte 8))
    (let* ((file-len (file-length in))
           (usb8 (make-array file-len
                             :element-type '(unsigned-byte 8)))
           (pos (read-sequence usb8 in)))
      (unless (= file-len pos)
        (error "Length read (~D) doesn't match file length (~D)~%"
               pos file-len))
      usb8)))

(defun read-stream-to-strings (in)
  (let ((lines '())
        (eof (gensym)))
    (do ((line (read-line in nil eof)
               (read-line in nil eof)))
        ((eq line eof))
      (push line lines))
    (nreverse lines)))

(defun read-file-to-strings (file)
  (with-open-file (in file :direction :input)
    (read-stream-to-strings in)))

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                         :if-exists :supersede)
      (stream-subst old new in out))))

(defun print-n-chars (char n stream)
  (declare (fixnum n)
           (optimize (speed 3)
                     (safety 0)
                     (space 0)))
  (dotimes (i n)
    (declare (fixnum i))
    (write-char char stream)))

(defun print-n-strings (str n stream)
  (declare (fixnum n)
           (optimize (speed 3)
                     (safety 0)
                     (space 0)))
  (dotimes (i n)
    (declare (fixnum i))
    (write-string str stream)))

(defun indent-spaces (n &optional (stream *standard-output*))
  (print-n-chars #\Space (+ n n) stream))

(defun indent-html-spaces (n &optional (stream *standard-output*))
  (print-n-strings "&nbsp;" (+ n n) stream))

(defun print-list (l &optional (stream *standard-output*))
  (format stream "~{~A~%~}" l))

(defun print-rows (rows &optional (stream *standard-output*))
  (dolist (r rows)
    (format stream "~{~A~^ ~}~%" r)))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Buffered stream substitute:

(defstruct buf
  vec
  (start -1)
  (used -1)
  (new -1)
  (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1
    (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
          (buf-new b) (buf-end b))))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new b) (buf-end b)))

(defun buf-clear (b)
  (setf (buf-start b) -1
        (buf-used b) -1
        (buf-new b) -1
        (buf-end b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))

(defun stream-subst (old new in out)
  (declare (string old new))
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (declare (fixnum pos len))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (declare (character c))
      (cond ((char= c (char old pos))
             (incf pos)
             (cond ((= pos len)            ; 3
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)         ; 2
                    (buf-insert c buf))))
            ((zerop pos)                   ; 1
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t                             ; 4
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

;;;}}}
;;; ------------------------------------------------------------------

(declaim (inline write-fixnum))
(defun write-fixnum (n s)
  #+allegro (excl::print-fixnum s 10 n)
  #-allegro (write-string (write-to-string n) s))

;;; Need to port this to Windows
(defun null-output-stream ()
  (when (probe-file #p"/dev/null")
    (open #p"/dev/null" :direction :output
          :if-exists :overwrite)))

(defun directory-tree (filename)
  (let* ((root (canonicalize-directory-name filename))
         (subdirs (loop for path in (directory
                                      (make-pathname :name :wild
                                                     :type :wild
                                                     :defaults root))
                        when (probe-directory path)
                          collect (canonicalize-directory-name path))))
    (when (find nil subdirs)
      (error "~A" subdirs))
    (when (null root)
      (error "~A" root))
    (if subdirs
        (cons root (mapcar #'directory-tree subdirs))
        (if (probe-directory root)
            (list root)
            (error "root not directory ~A" root)))))

;;; ------------------------------------------------------------------
;;;{{{ Writing stuff out:

(defmacro with-utime-decoding ((utime &optional zone) &body body)
  `(multiple-value-bind
       (second minute hour day-of-month month year day-of-week
               daylight-p zone)
       (decode-universal-time
        ,utime
        ,@(if zone (list zone)))
     (declare (ignorable second minute hour day-of-month month year
                         day-of-week daylight-p zone))
     ,@body))

(defvar +datetime-number-strings+
  (make-array 61
              :adjustable nil
              :element-type 'string
              :fill-pointer nil
              :initial-contents
              '("00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "10"
                "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21"
                "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32"
                "33" "34" "35" "36" "37" "38" "39" "40" "41" "42" "43"
                "44" "45" "46" "47" "48" "49" "50" "51" "52" "53" "54"
                "55" "56" "57" "58" "59" "60")))

(defun is-dst (utime)
  (with-utime-decoding (utime)
    daylight-p))

(defmacro with-utime-decoding-utc-offset ((utime utc-offset)
                                          &body body)
  (with-gensyms (zone)
    `(let* ((,zone (cond
                     ((eq :utc ,utc-offset)
                      0)
                     ((null utc-offset)
                      nil)
                     (t
                      (if (is-dst ,utime)
                          (1- (- ,utc-offset))
                          (- ,utc-offset))))))
       (if ,zone
           (with-utime-decoding (,utime ,zone)
             ,@body)
           (with-utime-decoding (,utime)
             ,@body)))))

(defun write-utime-hms (utime &key utc-offset stream)
  (if stream
      (write-utime-hms-stream utime stream utc-offset)
      (with-output-to-string (s)
        (write-utime-hms-stream utime s utc-offset))))

(defun write-utime-hms-stream (utime stream &optional utc-offset)
  (with-utime-decoding-utc-offset (utime utc-offset)
    (write-string (aref +datetime-number-strings+ hour) stream)
    (write-char #\: stream)
    (write-string (aref +datetime-number-strings+ minute) stream)
    (write-char #\: stream)
    (write-string (aref +datetime-number-strings+ second) stream)))

(defun write-utime-hm (utime &key utc-offset stream)
  (if stream
      (write-utime-hm-stream utime stream utc-offset)
      (with-output-to-string (s)
        (write-utime-hm-stream utime s utc-offset))))

(defun write-utime-hm-stream (utime stream &optional utc-offset)
  (with-utime-decoding-utc-offset (utime utc-offset)
    (write-string (aref +datetime-number-strings+ hour) stream)
    (write-char #\: stream)
    (write-string (aref +datetime-number-strings+ minute) stream)))

(defun write-utime-ymdhms (utime &key stream utc-offset)
  (if stream
      (write-utime-ymdhms-stream utime stream utc-offset)
    (with-output-to-string (s)
      (write-utime-ymdhms-stream utime s utc-offset))))

(defun write-utime-ymdhms-stream (utime stream &optional utc-offset)
  (with-utime-decoding-utc-offset (utime utc-offset)
    (write-string (prefixed-fixnum-string year nil 4) stream)
    (write-char #\/ stream)
    (write-string (aref +datetime-number-strings+ month) stream)
    (write-char #\/ stream)
    (write-string (aref +datetime-number-strings+ day-of-month) stream)
    (write-char #\space stream)
    (write-string (aref +datetime-number-strings+ hour) stream)
    (write-char #\: stream)
    (write-string (aref +datetime-number-strings+ minute) stream)
    (write-char #\: stream)
    (write-string (aref +datetime-number-strings+ second) stream)))

(defun write-utime-ymdhm (utime &key stream utc-offset)
  (if stream
      (write-utime-ymdhm-stream utime stream utc-offset)
      (with-output-to-string (s)
        (write-utime-ymdhm-stream utime s utc-offset))))

(defun write-utime-ymdhm-stream (utime stream &optional utc-offset)
  (with-utime-decoding-utc-offset (utime utc-offset)
    (write-string (prefixed-fixnum-string year nil 4) stream)
    (write-char #\/ stream)
    (write-string (aref +datetime-number-strings+ month) stream)
    (write-char #\/ stream)
    (write-string (aref +datetime-number-strings+ day-of-month) stream)
    (write-char #\space stream)
    (write-string (aref +datetime-number-strings+ hour) stream)
    (write-char #\: stream)
    (write-string (aref +datetime-number-strings+ minute) stream)))

(defun copy-binary-stream (in out &key (chunk-size 16384))
  (do* ((buf (make-array chunk-size :element-type '(unsigned-byte 8)))
        (pos (read-sequence buf in) (read-sequence buf in)))
       ((zerop pos))
    (write-sequence buf out :end pos)))

(defmacro with-input-from-file ((stream-name file-name
                                 &rest args
                                 &key (direction nil direction-provided-p)
                                 &allow-other-keys)
                                &body body)
  (declare (ignore direction))
  (when direction-provided-p
    (error "Can't specifiy :DIRECTION in WITH-INPUT-FROM-FILE."))
  `(with-open-file (,stream-name ,file-name :direction :input ,@args)
     ,@body))

(defmacro with-output-to-file ((stream-name file-name 
                                &rest args
                                &key (direction nil direction-provided-p)
                                &allow-other-keys)
                               &body body)
  (declare (ignore direction))
  (when direction-provided-p
    (error "Can't specifiy :DIRECTION in WITH-OUTPUT-FILE."))
  `(with-open-file (,stream-name ,file-name :direction :output ,@args)
     ,@body))

(defun read-file-into-string (pathname &key (buffer-size 4096)
                              (external-format :default))
  (with-input-from-file
      (file-stream pathname :external-format external-format)
    (let ((*print-pretty* nil))
      (with-output-to-string (datum)
        (let ((buffer (make-array buffer-size
                                  :element-type 'character)))
          (loop
            :for bytes-read = (read-sequence buffer file-stream)
            :do (write-sequence buffer datum :start 0 :end bytes-read)
            :while (= bytes-read buffer-size)))))))

(defun write-string-into-file (string pathname
                               &key (if-exists :error)
                               (if-does-not-exist :error)
                               (external-format
                                :default))
  (with-output-to-file
      (file-stream pathname
                   :if-exists if-exists
                   :if-does-not-exist
                   if-does-not-exist
                   :external-format external-format)
    (write-sequence string file-stream)))

(defun copy-buffer-stream (input output &optional
   (element-type (stream-element-type input)))
  (loop
    :with buffer-size = 4096
    :with buffer = (make-array buffer-size :element-type element-type)
    :for bytes-read = (read-sequence buffer input)
    :while (= bytes-read buffer-size)
    :do (write-sequence buffer output)
    :finally (write-sequence buffer output :end bytes-read)))

;;;}}}
;;; ------------------------------------------------------------------

;;; io.lisp ends here
