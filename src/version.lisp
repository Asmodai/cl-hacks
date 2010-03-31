;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; version.lisp --- Lisp identification functions.
;;;
;;; Time-stamp: <Tuesday Mar 30, 2010 12:40:19 asmodai>
;;; Revision:   81
;;;
;;; Copyright (c) 2009 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Tue Mar  7 17:44:34 2006
;;; Keywords:   Common Lisp Version Hacks
;;; URL:        http://unixware.kicks-ass.org/
;;;
;;; {{{ License:
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by
;;; the Free Software Foundation, as clarified by the Franz
;;; preamble to the LGPL found in
;;; http://opensource.franz.com/preamble.html.
;;;
;;; This code is distributed in the hope that it will be useful,
;;; but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License can be
;;; found at http://opensource.franz.com/license.html.
;;; If it is not present, you can access it from
;;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;;; version) or write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA  02111-1307  USA
;;;
;;; }}}
;;;
;;; {{{ Commentary:
;;;
;;; It appears that there is no real common "standard" for what
;;; the SOFTWARE-* and MACHINE-* functions return, so... let's define
;;; some functions that return common information across all Lisp
;;; platforms.
;;;
;;; This is probably the only module that resulted in me spending
;;; too much time trying out as many Lisp flavours as I could get my
;;; grubby hands on, now that's something to treasure for the rest
;;; of my life.  Sussman has his suit, I have cl-hacks.
;;;
;;; }}}

#-genera
(in-package "CL-HACKS")

;;; ===================================================================
;;; {{{ Lisp information:

;; If any systems here pre-date Common Lisp, or if any are missing...
;; let me know.
(defun common-lisp-implementation-type ()
  "Returns a string that identifies the generic name of the particular
Common Lisp implementation."
  (let ((lispm
	  #+abcl "Armed Bear Common Lisp"
	  #+(and akcl (not gcl)) "Austin Kyoto Common Lisp"
	  #+allegro "Franz Allegro Common Lisp"
	  #+clisp "GNU Common Lisp (CLISP)"
	  #+CLOE-Runtime "Symbolics CLOE"  ; Is this really needed?
	  #+cmu "CMU Common Lisp"
	  #+cormanlisp "Corman Common Lisp"
	  #+ecl "Embeddable Common Lisp"
	  #+(and excl (not allegro)) "Franz Common Lisp"
	  #+gcl "GNU Common Lisp (GCL)"
	  #+gclisp "Gold Hill Golden Common Lisp"
	  #+genera "Symbolics Common Lisp"
	  #+hp-hplabs "HP Common Lisp"
	  #+ibcl "Ibuki Common Lisp"
	  #+(and kcl (not gcl akcl)) "Kyoto Common Lisp"
	  #+lispworks "LispWorks"
	  #+Lucid "Lucid Common Lisp"
	  #+mcl "Macintosh Common Lisp"
	  #+(and openmcl (not clozure)) "OpenMCL"
	  #+clozure "Clozure Common Lisp"
	  #+poplog "Sussex Poplog Common Lisp"
	  #+pyramid "Pyramid Common Lisp"
	  #+sbcl "Steel Bank Common Lisp"
	  #+scl "Scieneer Common Lisp"
	  #+ti "Texas Instruments Common Lisp"
	  #+(and xerox medley) "Xerox InterLisp (Venue Medley)"
	  #+(and xerox (not medley)) "Xerox InterLisp"
	 ))
    lispm))

;; This is probably really gnarly and redundant... but... let's define
;; it anyway :)
(defun common-lisp-implementation-version ()
  "Returns a string that identifies the version of a particular Common Lisp
implementation"
  (common-lisp:lisp-implementation-version))
	 
;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ Software information:

;; GCL hackery is ugly isn't it.
(defun common-software-type ()
  "Returns a string that identifies the generic name of any relevant
supporting software, or nil if no appropriate or relevant result can
be produced.

Caveat:
GNU (Kyoto) Common Lisp (a.k.a GCL) seems to have both :LINUX and
:BSD in *FEATURES*.  I am assuming that the :LINUX symbol is not
present in BSD."
  (let ((os #+(and linux (not gcl)) "Linux"
	    #+sunos "SunOS"
	    #+osf1 "OSF/1"
	    #+(and gcl linux bsd) "Linux"
	    #+(and gcl bsd (not linux)) "BSD"
	    #+darwin "Darwin"
	    #+freebsd "FreeBSD"
	    #+openbsd "OpenBSD"
	    #+netbsd "NetBSD"
	    #+(or mswindows win32 win64) "Microsoft Windows"
	    #+(and unix (not (or bsd linux sunos osd1 darwin
				 freebsd openbsd netbsd)))
	    "UNIX"
	    #+genera
	    (progn
	      ;; This is quite complex and based in part on the code
	      ;; found in SYS:SYS2;HERALD.LISP.LATEST
	      (let ((is-open-genera-p #+VLM t #-VLM nil)
		    (is-genera-p t))
		(multiple-value-bind (major minor status open-genera)
		    (sct:get-release-version)
		  (declare (ignore minor))
		  (setq is-open-genera-p (and (member :vlm cl::*features*)
					    (not (null open-genera))))
		  (when (or (null major)
			    (eq status :interim))
		    (setq is-open-genera-p nil
			  is-genera-p nil))
		  (format nil "Symbolics ~:[System~;~:[Genera~;Open Genera~]~]"
			  is-genera-p is-open-genera-p))))
	    ))
    os))

;; TODO: Port this to Windows.
(defun common-software-version ()
  "Returns a string that identifies the version of any relevant
supporting software, or nil if no appropriate or relevant result can
be produced."
  #+unix
  (multiple-value-bind (output error status)
      (command-output
       #+(or bsd darwin) "/usr/bin/uname -r"
       #-(and bsd darwin) "/bin/uname -r")
    (declare (ignore error status))
    (strip-newlines output))
  #+genera
  (progn
    (let ((major-version nil)
	  (minor-version nil)
	  (release-status nil)
	  (open-genera-version nil)
	  (open-genera-p nil))
      (multiple-value-bind (major minor status open-genera)
	  (sct:get-release-version)
	(setq major-version major
	      minor-version minor
	      release-status status)
	(if (and (member :vlm cl::*features*)
		 (not (null open-genera)))
	    (setq open-genera-p t
		  open-genera-version open-genera))
	(when (or (null major)
		  (eq status :interim))
	  (multiple-value-bind (major minor status)
	      (sct:get-system-version)
	    (setq major-version major
		  minor-version minor
		  release-status status))))
      (if open-genera-p
	  (format nil "~a" open-genera-version)
	  (format nil "~d.~d" major-version minor-version))))
  #-(or genera unix)
  nil
  )

;;; }}}
;;; ===================================================================

;;; ===================================================================
;;; {{{ Machine information:


;; TODO: Add support for Windows.
(defun common-machine-version ()
  "Ascertains and returns the machine version from either the operating
system or the underlaying hardware."
  #+(and linux (not abcl))
  (with-open-file (stream "/proc/cpuinfo"
			  :if-does-not-exist nil)
    (loop with line while (setf line (read-line stream nil))
	  when (eql (search "model name" line) 0)
	    return (string-trim " " (subseq line
					    (1+ (position #\: line))))))
  #+(or bsd darwin)
  (multiple-value-bind (output error status)
      (command-output 
       #+darwin "/usr/sbin/sysctl machdep.cpu.brand_string"
       #+bsd "/sbin/sysctl hw.model")
    (declare (ignore error status))
    (string-left-trim-whitespace
     (second
      (delimited-string-to-list (strip-newlines output) #\:))))
  #+abcl
  (java:jstatic "getProperty" "java.lang.System" "java.version")
  #+(and genera (not vlm))
  (common-lisp:machine-version)
  #+(and genera vlm)
  "Unix Workstation"
  #-(or linux abcl bsd darwin genera)
  "Unknown")

(defun common-machine-type ()
  "Ascertains and returns the machine type either using *FEATURES*
or from the OS and/or hardware."
  (let ((mtype "Unknown"))
    #+abcl
    (setq mtype "Java Virtual Machine")
    #+genera
    (progn
      (let ((model (si:machine-model)))
	(cond ((eq model :unknown)
	       (setq mtype "Symbolics Lisp Machine"))
	      (t
	       (setq mtype (format nil "Symbolics ~d" model))))))
    #+(or x86 pc386 pc iapx386 i386 i486 i586 i686 pentium
	  pentiummmx pentiumpro pentium2 pentium3 pentium4)
    (setf mtype "x86")
    #+(or x86-64 x86_64)
    (setf mtype "x86-64")
    #+(or ppc powerpc)
    (setf mtype "PowerPC")
    mtype))

;; This is probably redundant too :)
(defun common-machine-instance ()
  "Returns a string that identifies the particular instance of the computer hardware
on which Common Lisp is running."
  (common-lisp:machine-instance))

;;; }}}
;;; ===================================================================

;;; version.lisp ends here

