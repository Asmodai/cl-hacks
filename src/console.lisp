;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; console.lisp --- Various console utilities
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:41:49 asmodai>
;;; Revision:   12
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2002 Keven M. Rosenberg
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Tue Sep  1 19:00:00 2009
;;; Keywords:   Common Lisp Console Hacks
;;; URL:        http://unixware.kicks-ass.org/
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

#-genera
(in-package #:cl-hacks)

(defvar *console-messages* t
  "Define to T if console messages are to be displayed.")

(defvar *console-message-types* nil
  "Add console message types that you want to display via CMSG-C here.
e.g. something like :DEBUG will show console messages when you use
(CMSG-C :DEBUG ``foo'')")

(defun cmsg (template &rest args)
  "Format output to console."
  (when *console-messages*
    (setq template (concatenate 'string "~&;; " template "~%"))
    (apply 'format *standard-output* template args)))

(defun cmsg-c (condition template &rest args)
  "Push CONDITION keywords into *CONSOLE-MESSAGE-TYPES* to print
console messages for that CONDITION.  TEMPLATE and ARGS function
indentically to (FORMAT *STANDARD-OUTPUT* TEMPLATE ARGS)."
  (when (or (member :verbose *console-message-types*)
            (member condition *console-message-types*))
    (apply #'cmsg template args)))

(defun cmsg-add (condition)
  "Add a new condition to the list of console message types."
  (pushnew condition *console-message-types*))

(defun cmsg-remove (condition)
  "Remove a condition from the list of console message types."
  (setf *console-message-types* (remove condition *console-message-types*)))

(defun cmsg-list ()
  "List available console message types."
  *console-message-types*)

#-genera
(defun fixme (template &rest args)
  "Display a FIXME message."
  (setq template (concatenate 'string "~&;; ** FIXME ** " template "~&"))
  (apply #'format *standard-output* template args)
  (values))

#+genera
(defun fixme (template &rest args)
  (let ((window (si:follow-syn-stream zl:standard-output)))
    (scl:with-character-style ('(:swiss :roman :normal) window
                                :bind-line-height t)
      (format window "~&;; ")
      (scl:with-character-face (:bold window)
        (format window "** FIXME ** "))
      (scl:with-character-face (:italic window)
        (apply #'format window template args)))))

;;; console.lisp ends here
