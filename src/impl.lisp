;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; impl.lisp --- Implementation-specific hacks
;;;
;;; Time-stamp: <Monday Mar 29, 2010 12:29:21 asmodai>
;;; Revision:   45
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

(defun canonicalize-directory-name (filename)
  (flet ((un-unspecific (value)
	   (if (eq value :unspecific)
	       nil
	       value)))
    (let* ((path (pathname filename))
	   (name (un-unspecific (pathname-name path)))
	   (type (un-unspecific (pathname-type path)))
	   (new-dir
	     (cond ((and name type)
		    (list (concatenate 'string name "." type)))
		   (name
		    (list name))
		   (type
		    (list type))
		   (t
		    nil))))
      (if new-dir
	  (make-pathname :directory (append (un-unspecific
					      (pathname-directory path))
					    new-dir)
			 :name nil
			 :type nil
			 :version nil
			 :defaults path)
	  path))))

#-genera
(defun probe-directory (filename &key (error-if-does-not-exist nil))
  (let* ((path (canonicalize-directory-name filename))
	 (probe
	   #+allegro (excl:probe-directory path)
	   #+clisp (values
		     (ignore-errors
		       (#+lisp=cl ext:probe-directory
			#-lisp=cl lisp:probe-directory
			path)))
	   #+(or cmu scl) (when (eq :directory
				    (unix:unix-file-kind
				      (namestring path)))
			    path)
	   #+lispworks (when (lw:file-directory-p path)
			 path)
	   #+sbcl (let ((file-kind-fun
			  (or (find-symbol "NATIVE-FILE-KIND" :sb-impl)
			      (find-symbol "UNIX-FILE-KIND" :sb-unix))))
		    (when (eq :directory (funcall file-kind-fun
						  (namestring path)))
		      path))
	   #-(or allegro clisp cmu lispworks sbcl scl)
	   (probe-file path)))
    (if probe
	probe
	(when error-if-does-not-exist
	  (error "Directory ~a does not exist." filename)))))

#+genera
(defun probe-directory (filename &key (error-if-does-not-exist nil))
  ;; Force an :error keyword for future-common-lisp:open
  (when error-if-does-not-exist
    (setf error-if-does-not-exist :error))
  (flet ((probe-it (pathname)
	   (loop with path = (pathname pathname)
		 with host = (pathname-host path)
		 with device = (pathname-device path)
		 for dir in (pathname-directory path :case :local)
		 collect dir into directories
		 when (and (cdr directories)
			   (not (open (make-pathname :host host
						     :device device
						     :directory
						     directories)
				      :direction :probe-directory
				      :if-does-not-exist error-if-does-not-exist)))
		   do (return nil)
		 finally (return t))))
    (declare (inline probe-it))
    ;;
    ;; OpenGenera seems to ignore :IF-DOES-NOT-EXIST NIL and pump out
    ;; an error anyway.  As this isn't the kind of behaviour we want, 
    ;; we will silently ignore any error with the CONDITIONS package
    ;; unless we specifically want errors :)
    (if error-if-does-not-exist
	(probe-it filename)
	(conditions:handler-case
	  (probe-it filename)
	  (fs:directory-not-found () nil)))))

#-genera
(defun cwd (&optional dir)
  (cond ((not (null dir))
	 (when (and (typep dir 'logical-pathname)
		    (translate-logical-pathname dir))
	   (setq dir (translate-logical-pathname dir)))
	 (when (stringp dir)
	   (setq dir (parse-namestring dir)))
	 #+allegro (excl:chdir dir)
	 #+clisp (#+lisp=cl ext:cd #-lisp=cl lisp:cd dir)
	 #+(or cmu scl) (setf (ext:default-directory) dir)
	 #+cormanlisp (ccl:set-current-directory dir)
	 #+(and mcl (not openmcl)) (ccl:set-mac-default-directory dir)
	 #+openmcl (ccl:cwd dir)
	 #+gcl (si:chdir dir)
	 #+lispworks (hcl:change-directory dir)
	 (setq cl:*default-pathname-defaults* dir))
	(t
	 (let ((dir
		 #+allegro (excl:current-directory)
		 #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
		 #+(or cmu scl) (ext:default-directory)
		 #+sbcl (sb-unix:posix-getcwd/)
		 #+cormanlisp (ccl:get-current-directory)
		 #+lispworks (hcl:get-working-directory)
		 #+mcl (ccl:mac-default-directory)
		 #-(or allegro clisp cmu scl cormanlisp mcl sbcl lispworks)
		 (truename ".")))
	   (when (stringp dir)
	     (setq dir (parse-namestring dir)))
	   dir))))

;;; This is the most hairy hack you have ever seen.  As Genera has no
;;; real notion of `current working directory', we have to kludge it.
;;; CL:*DEFAULT-PATHNAME-DEFAULTS* usually contains the default filespec
;;; for when you create a new file buffer in ZMACS or whatnot... which
;;; is not what we want, as it is not the *current* directory you're
;;; currently looking at.  So, we need to go deep into Dynamic Windows
;;; here... very deep... so deep that we can easily stumble across
;;; dwarves as they go about their daily business.  Hopefully we won't
;;; stumble into any drow though.
#+genera
(defun cwd (&optional dir)
  (cond ((not (null dir))
	 (when (and (typep dir 'logical-pathname)
		    (translate-logical-pathname dir))
	   (setq dir (translate-logical-pathname dir)))
	 (when (stringp dir)
	   (setq dir (parse-namestring dir)))
	 ;;
	 ;; All we do here is append a new pathname to the pathname
	 ;; history
	 (let ((history (dw::find-accept-history 'fs:pathname)))
	   (if (not history)
	       (error "Cannot put ~S on the Dynamic Windows history."
		      dir)
	       (dw::push-for-presentation-type history dir 'pathname ""))))
	(t
	 (let ((dir (scl:send (si:pathname-history-first-pathname)
			      :new-pathname
			      :name nil
			      :type nil
			      :version :newest)))
	   dir))))

(defun quit (&optional (code 0))
  (declare (ignorable code))
  #+allegro (excl:exit code :quiet t)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+(or cmu scl) (ext:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #+sbcl (sb-ext:quit :unix-status (typecase code (number code) (null 0)
					     (t 1)))
  #+mcl (ccl:quit code)
  #+genera (error "You're trying to `quit' on a Lisp Machine? Nuts!")
  #-(or allegro clisp cmu scl cormanlisp gcl lispworks lucid sbcl mcl
	     genera)
  (error 'not-implemented :proc (list 'quit code)))

(defun command-line-arguments ()
  #+allegro (system:command-line-arguments)
  #+sbcl sb-ext:*posix-argv*
  )

;;; Genera already has a COPY-FILE, and we *do not* want to clobber that :)
#-genera
(defun copy-file (from to &key link overwrite preserve-symbolic-links
		  (preserve-time t) remove-destination force verbose)
  #+allegro
  (sys:copy-file from
		 to
		 :link link
		 :overwrite overwrite
		 :preserve-symbolic-links preserve-symbolic-links 
		 :preserve-time preserve-time
		 :remove-destination remove-destination
		 :force force
		 :verbose verbose)
  #-allegro
  (declare (ignore verbose preserve-symbolic-links overwrite))
  (cond
    ((and (typep from 'stream) (typep to 'stream))
     (copy-binary-stream from to))
    ((not (probe-file from))
     (error "File ~a does not exist." from))
    ((eq link :hard)
     (run-shell-command "/bin/ln -f ~a ~a"
			(namestring from)
			(namestring to)))
    (link
     (multiple-value-bind (stdout stderr status)
	 (command-output "/bin/ln -f ~a ~a"
			 (namestring from)
			 (namestring to))
       (declare (ignore stdout stderr))
       (unless (zerop status)
	 (run-shell-command "/bin/ln -s ~a ~a"
			    (namestring from)
			    (namestring to)))))
    (t
     (when (and (or force remove-destination) (probe-file to))
       (delete-file to))
     (let* ((options (if preserve-time
			 "-p"
			 ""))
	    (cmd (format nil "/bin/cp ~a ~a ~a"
			 options
			 (namestring from)
			 (namestring to))))
       (run-shell-command cmd)))))

;;; impl.lisp ends here

