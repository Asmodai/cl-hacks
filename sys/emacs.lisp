;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; emacs.lisp --- Functionality for Lisp Emacsen and other editors.
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:50:03 asmodai>
;;; Revision:   11
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 15:38:11
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
;;; For the most part, the indentation rules are a list of offsets in
;;; the format of (no-of-sexprs-to-skip amount-to-change-indentation).
;;;
;;;}}}

#-genera
(in-package #:cl-hacks-internals)

;;; ==================================================================
;;;{{{ Implementation-specific functions:

;;;
;;; Genera provides ZWEI:DEFINDENTATION and ZWEI:GET-INDENTATION to set
;;; and get the ZWEI indentation for symbols.
;;;

#+(or lispworks allegro)
(pushnew :cl-hacks-emacs cl:*features*)

;;; ------------------------------------------------------------------
;;;{{{ LispWorks:

;;; This function will add an indentation rule to the LispWorks editor
;;; substrate.

#+lispworks
(defun %defindentation (name indentation)
  (when (symbolp name)
    (let ((fun (append (list 'editor:setup-indent (string name))
                       indentation)))
      (eval fun))))

#+lispworks
(defun %get-indentation (thing)
  (let* ((table editor::*default-syntax-table*)
         (operators (slot-value table 'editor::operators)))
    (coerce
     (gethash (string-upcase (string thing)) operators)
     'list)))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Franz Allegro:

;; This function will add an indentation rule to the Allegro Common Lisp
;; editor substrate.
#+allegro
(defun %defindentation (name indentation)
  (when (symbolp name)
    (setf (cg:text-edit-indentation name) indentation)))


#+allegro
(defun %get-indentation (&rest thing)
  (list s (cg:text-edit-indentation thing)))

;;;}}}
;;; ------------------------------------------------------------------

;;; Todo: more editor substrates?  DREI? Hemlock? Climacs?

;;; Wrapper macros.
#+cl-hacks-emacs
(progn
  (defmacro defindentation ((name &rest indentation))
    `(%defindentation ',name ',indentation))

  (defmacro get-indentation (thing)
    `(%get-indentation ,thing)))

;;; Wrapper macros for when we're in a Lisp environment that does not
;;; support editing indentation.
#-cl-hacks-emacs
(progn
  (defmacro defindentation ((name &rest indentation))
    (declare (ignore name indentation)))

  (defmacro get-indentation (thing)
    (declare (ignore thing))))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Indentation rules for various CL-HACKS forms:

;;;}}}
;;; ==================================================================

;;; emacs.lisp ends here
