;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; collecting.lisp --- Collecting lists forwards
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:41:38 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 1989-2002 Tim Bradshaw
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 16:38:00
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
;;; Collecting lists forwards
;;; This is an old macro cleaned up a bit
;;;
;;; These macros hardly seem worth copyrighting, but are copyright
;;; 1989-2000 by me, Tim Bradshaw, and may be used for any purpose
;;; whatsoever by anyone. There is no warranty whatsoever. I would
;;; appreciate acknowledgement if you use this in anger, and I would
;;; also very much appreciate any feedback or bug fixes.
;;;
;;;}}}

#-genera
(in-package #:cl-hacks)

;;; Collect some random stuff into a list by keeping a tail-pointer
;;; to it, return the collected list.  No real point in using
;;; gensyms, although one probably should on principle.
(defmacro collecting (&body forms)
  (let (($resnam$ (gensym)) ($tail$ (gensym)) ($thing$ (gensym)))
    `(let (,$resnam$ ,$tail$)
       (macrolet
           ((collect
                (thing)
              ;; Collect returns the thing it's collecting
              `(let ((,',$thing$ ,thing))
                 (if ,',$resnam$
                     (setf (cdr ,',$tail$)
                           (setf ,',$tail$ (list ,',$thing$)))
                     (setf ,',$resnam$
                           (setf ,',$tail$ (list ,',$thing$))))
                 ,',$thing$)))
         ,@forms)
       ,$resnam$)))

;;; multiple-collector version of COLLECTING.
(defmacro with-collectors ((&rest collectors) &body forms)
  (let ((cvns (mapcar
               #'(lambda (c)
                   (make-symbol
                    (concatenate 'string (symbol-name c) "-VAR")))
               collectors))
        (ctns (mapcar
               #'(lambda (c)
                   (make-symbol
                    (concatenate 'string (symbol-name c) "-TAIL")))
               collectors)))
    `(let (,@cvns ,@ctns)
       (macrolet ,(mapcar 
                   #'(lambda (cn cvn ctn)
                       `(,cn (v)
                             (let ((vn (make-symbol "V")))
                               `(let ((,vn ,v))
                                  (if ,',cvn
                                      (setf (cdr ,',ctn)
                                            (setf ,',ctn (list ,vn)))
                                      (setf ,',cvn 
                                            (setf ,',ctn (list ,vn))))
                                  ,vn))))
                   collectors cvns ctns)
         ,@forms
         (values ,@cvns)))))

;;; collecting.lisp ends here

