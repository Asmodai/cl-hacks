;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; os.lisp --- OS-specific functions
;;;
;;; Time-stamp: <Tuesday Mar 30, 2010 11:20:43 asmodai>
;;; Revision:   13
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
(in-package "CL-HACKS")

(defun command-output (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell,
returns (VALUES string-output error-output exit-status)"
  (let ((command (apply #'format nil control-string args)))
    ;;
    ;; Armed Bear -- Does this work?
    ;;    #+abcl
    ;;   (let* ((process (extensions:run-shell-comand
    ;;    (format nil "/bin/sh -c ~a" command))
    ;;
    ;; SBCL
    #+sbcl
    (let* ((process (sb-ext:run-program "/bin/sh"
					(list "-c" command)
					:input nil
					:output :stream
					:error :stream))
           (output (read-stream-to-string (sb-impl::process-output process)))
           (error (read-stream-to-string (sb-impl::process-error process))))
      (close (sb-impl::process-output process))
      (close (sb-impl::process-error process))
      (values output
	      error
	      (sb-impl::process-exit-code process)))
    ;;
    ;; CMU and Scieneer
    #+(or cmu scl)
    (let* ((process (ext:run-program "/bin/sh"
				     (list "-c" command)
				     :input nil 
				     :output :stream 
				     :error :stream))
           (output (read-stream-to-string (ext::process-output process)))
           (error (read-stream-to-string (ext::process-error process))))
      (close (ext::process-output process))
      (close (ext::process-error process))
      (values output
	      error
	      (ext::process-exit-code process)))
    ;;
    ;; Allegro
    #+(and allegro unix)
    (multiple-value-bind (output error status)
        (excl.osi:command-output command :whole t)
      (values output error status))
    ;;
    ;; Lispworks
    #+lispworks
    ;; BUG: Lispworks combines output and error streams
    (let ((output (make-string-output-stream)))
      (unwind-protect
	   (let ((status
		  (system:call-system-showing-output
		   command
		   :prefix ""
		   :show-cmd nil
		   :output-stream output)))
	     (values (get-output-stream-string output) nil status))
        (close output)))
    ;;
    ;; CLISP
    #+clisp
    ;; BUG: CLisp doesn't allow output to user-specified stream
    (values
     nil
     nil
     (ext:run-shell-command command :output :terminal :wait t))
    ;;
    ;; OpenMCL
    #+openmcl
    (let* ((process (ccl:run-program "/bin/sh"
				     (list "-c" command)
				     :input nil
				     :output :stream 
				     :error :stream
				     :wait t))
           (output (read-stream-to-string (ccl::external-process-output-stream process)))
           (error (read-stream-to-string (ccl::external-process-error-stream process))))
      (close (ccl::external-process-output-stream process))
      (close (ccl::external-process-error-stream process))
      (values output
              error
              (nth-value 1 (ccl::external-process-status process))))
    ;;
    ;; Everything else
    #-(or openmcl clisp lispworks allegro scl cmu sbcl abcl)
    (error "COMMAND-OUTPUT not implemented.")))

(defun run-shell-command (program &rest args)
  "This function provides a quick and dirty interface to COMMAND-OUTPUT, but it will only
return the exit status."
  (let ((command (apply #'format nil program args)))
    (multiple-value-bind (output error status)
	(command-output command)
      (declare (ignore output error))
      status)))

(defun delete-directory-and-files (dir &key (if-does-not-exist :error) (quiet t) force)
  #+allegro (excl:delete-directory-and-files dir :if-does-not-exist if-does-not-exist
                                             :quiet quiet :force force)
  #-(or allegro) (declare (ignore force))
  #-(or allegro) (cond
                   ((probe-directory dir)
                    (let ((cmd (format nil "rm -rf ~A" (namestring dir))))
                      (unless quiet
                        (format *trace-output* ";; ~A" cmd))
                      (command-output cmd)))
                   ((eq if-does-not-exist :error)
                    (error "Directory ~A does not exist [delete-directory-and-files]." dir))))
(defun file-size (file)
  (when (probe-file file)
    #+allegro (let ((stat (excl.osi:stat (namestring file))))
                (excl.osi:stat-size stat))
    #-allegro
    (with-open-file (in file :direction :input)
      (file-length in))))

(defun getpid ()
  "Return the PID of the lisp process."
  #+allegro (excl::getpid)
  #+(and lispworks win32) (win32:get-current-process-id)
  #+(and lispworks (not win32)) (system::getpid)
  #+sbcl (sb-posix:getpid)
  #+cmu (unix:unix-getpid)
  #+openmcl (ccl::getpid)
  #+(and clisp unix) (system::process-id)
  #+(and clisp win32) (cond ((find-package :win32)
                             (funcall (find-symbol "GetCurrentProcessId"
                                                   :win32)))
                            (t
                             (system::getenv "PID"))))

;;; os.lisp ends here
