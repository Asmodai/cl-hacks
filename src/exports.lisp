;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; exports.lisp --- Package exports
;;;
;;; Time-stamp: <Friday Dec  9, 2011 06:48:13 asmodai>
;;; Revision:   11
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    01 Dec 2011 13:56:03
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

;;;
;;; Export everything that is an exported symbol in the
;;; CL-HACKS-INTENRALS, CL-HACKS-MOP, and CL-HACKS-CLOS packages.
;;;
;;; This makes CL-HACKS a Single Unified Package for Everything(tm).
;;;

(eval-when (:load-toplevel :compile-toplevel :execute)
  (let ((mypkg (find-package :cl-hacks)))
    (if (null mypkg)
        (error "Could not find CL-HACKS package while processing ~
                exports!"))
    (flet ((exp-vars (package)
             (do-symbols (s package)
               (multiple-value-bind (sym status)
                   (find-symbol (symbol-name s) package)
                 (when (and (eq status :external)
                            (boundp sym))
                   (format *standard-output*
                           "~&;  -> Exporting variable ~A:~S.~%"
                           (package-name package)
                           sym)
                   (export sym mypkg)))))
           (exp-funs (package)
             (do-symbols (s package)
               (multiple-value-bind (sym status)
                   (find-symbol (symbol-name s) package)
                 (when (and (eq status :external)
                            (fboundp sym))
                   (format *standard-output*
                           "~&;  -> Exporting function ~A:~S.~%"
                           (package-name package)
                           sym)
                   (export sym mypkg))))))
      ;;
      ;; Tell the user what's going on
      (format *standard-output*
              "~&; Exporting symbols from various packages used by ~
                 CL-HACKS.~%")
      ;;
      ;; Process the packages
      (loop for pkg in '("CL-HACKS-INTERNALS"
                         "CL-FAD"
                         "CL-HACKS-MOP"
                         "CL-HACKS-CLOS")
            doing (let ((fpkg (find-package pkg)))
                    (if (not (null fpkg))
                        (progn
                          (format *standard-output*
                                  "~&; Processing ~S:~%"
                                  (package-name fpkg))
                          (exp-vars fpkg)
                          (exp-funs fpkg))
                        (warn "~S not found." pkg)))))))

;;; exports.lisp ends here
