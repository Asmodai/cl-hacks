;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;;;
;;; package.lisp --- FAD package definition
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:40:27 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    02 Dec 2011 06:00:22
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
(in-package #:cl-user)

(defpackage #:cl-hacks-fad
  (:nicknames :cl-fad :fad)
  (:use #+genera #:future-common-lisp
        #-genera #:common-lisp)
  ;;
  ;; Shadows for Genera
  #+genera
  (:shadow #:copy-file)
  ;;
  ;; Shadows for Allegro CL
  #+allegro
  (:shadow #:copy-file
           #:delete-directory-and-files)
  ;;
  ;; Shadows for ABCL
  #+abcl
  (:shadow #:list-directory)
  ;;
  ;; Exports
  (:export #:copy-file
           #:copy-stream
           #:delete-directories-and-files
           #:directory-exists-p
           #:directory-pathname-p
           #:file-exists-p
           #:list-directory
           #:pathname-as-directory
           #:pathname-as-file
           #:walk-directory))

;;; package.lisp ends here
