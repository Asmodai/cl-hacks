;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-HACKS; Base: 10; Lowercase: Yes -*-
;;;
;;; strings.lisp --- String functions
;;;
;;; Time-stamp: <Wednesday Mar 31, 2010 18:22:23 asmodai>
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
(in-package "CL-HACKS")

;; Danger Will Robinson:  a STRING-APPEND function already exists in
;; Genera, clobbering it will NOT do the lispm any good
#-genera
(defmacro string-append (str &rest args)
  `(setq ,str (concatenate 'string ,str ,@args)))

(defun list-to-string (lst)
  (format nil "~{~a~}" lst))

(defun count-string-words (str)
  (declare (simple-string str)
	   (optimize (speed 3)
		     (safety 0)
		     (space 0)))
  (let ((n-words 0)
	(in-word nil))
    (declare (fixnum n-words))
    (do* ((len (length str))
	  (i 0 (1+ i)))
	 ((= i len) n-words)
      (declare (fixnum i))
      (if (alphanumericp (schar str i))
	  (unless in-word
	    (incf n-words)
	    (setq in-word t))
	  (setq in-word nil)))))

(defun position-char (char string start max)
  (declare (optimize (speed 3)
		     (safety 0)
		     (space 0))
	   (fixnum start max)
	   (simple-string string))
  (do* ((i start (1+ i)))
       ((= i max) nil)
    (declare (fixnum i))
    (when (char= char (schar string i))
      (return i))))

(defun position-not-char (char string start max)
  (declare (optimize (speed 3)
		     (safety 0)
		     (space 0))
	   (fixnum start max)
	   (simple-string string))
  (do* ((i start (1+ i)))
       ((= i max) nil)
    (declare (fixnum i))
    (when (char/= char (schar string i))
      (return i))))

(defun delimited-string-to-list (string &optional (separator #\Space)
				 skip-terminal)
  (declare (optimize (speed 3)
		     (safety 0)
		     (space 0)
		     (compilation-speed 0))
	   (type string string)
	   (type character separator))
  (do* ((len (length string))
	(output '())
	(pos 0)
	(end (position-char separator string pos len)
	     (position-char separator string pos len)))
       ((null end)
	(if (< pos len)
	    (push (subseq string pos) output)
	    (when (or (not skip-terminal) (zerop len))
	      (push "" output)))
	(nreverse output))
    (declare (type fixnum pos len)
	     (type (or null fixnum) end))
    (push (subseq string pos end) output)
    (setq pos (1+ end))))

(defun list-to-delimited-string (list &optional (separator ""))
  (format nil (concatenate 'string "~{~a~^" (string separator) "~}")
	  list))

#-genera
(defun string-invert (str)
  (declare (optimize (speed 3)
		     (compilation-speed 0)
		     (debug 0)
		     (safety 0))
	   (simple-string str))
  (let ((up nil) (down nil))
    (block skip
      (loop for char of-type character across str do
	(cond ((upper-case-p char)
	       (if down (return-from skip str) (setf up t)))
	      ((lower-case-p char)
	       (if up (return-from skip str) (setf down t)))))
      (if up (string-downcase str) (string-upcase str)))))

#+genera
(defun string-invert (str)
  (declare (optimize (speed 3)
		     (compilation-speed 0)
		     (debug 0)
		     (safety 0))
	   (simple-string str))
  (let ((up nil) (down nil))
    (block skip
      (loop for i fixnum from 0 below (length str)
	    with char = (char str i)
	    do (cond ((upper-case-p char)
		      (if down
			  (return-from skip str)
			  (setf up t)))
		     ((lower-case-p char)
		      (if up
			  (return-from skip str)
			  (setf down t)))))
      (if up
	  (string-downcase str)
	  (string-upcase str)))))

(defun add-sql-quotes (s)
  (substitute-string-for-char s #\' "''"))

(defun escape-backslashes (s)
  (substitute-string-for-char s #\\ "\\\\"))

(defun substitute-string-for-char (procstr match-char subst-str)
  (substitute-chars-strings procstr (list (cons match-char subst-str))))

(defun string-substitute (string substring replacement-string)
  "String substitution by Larry Hunter.  Obtained from Google."
  (let ((substring-length (length substring))
	(last-end 0)
	(new-string ""))
    (do ((next-start
	   (search substring string)
	   (search substring string :start2 last-end)))
	((null next-start)
	 (concatenate 'string new-string (subseq string last-end)))
      (setq new-string
	    (concatenate 'string
			 new-string
			 (subseq string last-end next-start)
			 replacement-string))
      (setq last-end (+ next-start substring-length)))))

(defun string-trim-last-character (s)
  (let ((len (length s)))
    (if (plusp len)
	(subseq s 0 (1- len))
	s)))

(defun nstring-trim-last-character (s)
  (let ((len (length s)))
    (if (plusp len)
	(nsubseq s 0 (1- len))
	s)))

(defun string-hash (str &optional (bitmask 65535))
  (let ((hash 0))
    (declare (fixnum hash)
	     (simple-string str))
    (dotimes (i (length str))
      (declare (fixnum i))
      (setq hash (+ hash (char-code (char str i)))))
    (logand hash bitmask)))

(defun is-string-empty (str)
  (zerop (length str)))

(defvar *whitespace-chars* '(#\Space #\Tab #\Return #\Linefeed
			     #+allegro #\%space
			     #+lispworks #\No-Break-Space))

(defun is-char-whitespace (c)
  (declare (character c)
	   (optimize (speed 3)
		     (safety 0)))
  (or (char= c #\Space)
      (char= c #\Tab)
      (char= c #\Return)
      (char= c #\Linefeed)
      #+allegro (char c #\%space)
      #+lispworks (char= c #\No-Break-Space)))

(defun is-string-whitespace (str)
  (every #'is-char-whitespace str))

(defun string-right-trim-whitespace (str)
  (string-right-trim *whitespace-chars* str))

(defun string-left-trim-whitespace (str)
  (string-left-trim *whitespace-chars* str))

(defun string-trim-whitespace (str)
  (string-trim *whitespace-chars* str))

(defun replaced-string-length (str repl-alist)
  (declare (simple-string str)
	   (optimize (speed 3)
		     (safety 0)
		     (space 0)))
  (do* ((i 0 (1+ i))
	(orig-len (length str))
	(new-len orig-len))
       ((= i orig-len) new-len)
    (declare (fixnum i orig-len new-len))
    (let* ((c (char str i))
	   (match (assoc c repl-alist :test #'char=)))
      (declare (character c))
      (when match
	(incf new-len
	      (1- (length (the simple-string (cdr match)))))))))

(defun substitute-chars-strings (str repl-alist)
  (declare (simple-string str)
	   (optimize (speed 3)
		     (safety 0)
		     (space 0)))
  (do* ((orig-len (length str))
	(new-string (make-string
		      (replaced-string-length str repl-alist)))
	(spos 0 (1+ spos))
	(dpos 0))
       ((>= spos orig-len)
	new-string)
    (declare (fixnum spos dpos)
	     (simple-string new-string))
    (let* ((c (char str spos))
	   (match (assoc c repl-alist :test #'char=)))
      (declare (character c))
      (if match
	  (let* ((subst (cdr match))
		 (len (length subst)))
	    (declare (fixnum len)
		     (simple-string subst))
	    (dotimes (j len)
	      (declare (fixnum j))
	      (setf (char new-string dpos) (char subst j))
	      (incf dpos)))
	  (progn
	    (setf (char new-string dpos) c)
	    (incf dpos))))))

(defun escape-xml-string (string)
  (substitute-chars-strings string '((#\& . "&amp;")
				     (#\< . "&lt;")
				     (#\> . "&rt;"))))

(defun make-usb8-array (len)
  (make-array len :element-type '(unsigned-byte 8)))

(defun usb8-array-to-string (vec &key (start 0) end)
  (declare (type (simple-array (unsigned-byte 8) (*)) vec)
	   (fixnum start))
  (unless end
    (setq end (length vec)))
  (let* ((len (- end start))
	 (str (make-string len)))
    (declare (fixnum len)
	     (simple-string str)
	     (optimize (speed 3)
		       (safety 0)))
    (do ((i 0 (1+ i)))
	((= i len) str)
      (declare (fixnum i))
      (setf (schar str i)
	    (code-char (aref vec
			     (the fixnum (+ i start))))))))  

(defun string-to-usb8-array (str)
  (declare (simple-string str))
  (let* ((len (length str))
	 (vec (make-usb8-array len)))
    (declare (fixnum len)
	     (type (simple-array (unsigned-byte 8) (*)) vec)
	     (optimize (speed 3)))
    (do ((i 0 (1+ i)))
	((= i len) vec)
      (declare (fixnum i))
      (setf (aref vec i) (char-code (schar str i))))))

(defun concat-separated-strings (sep &rest lists)
  (format nil (concatenate 'string "~{~A~^" (string sep) "~}")
	  (append-sublists lists)))

(defun only-null-list-elements-p (lst)
  (or (null lst) (every #'null lst)))

(defun print-separated-strings (strm sep &rest lists)
  (declare (optimize (speed 3)
		     (safety 0)
		     (space 0)
		     (debug 0)
		     (compilation-speed 0)))
  (do* ((rest-lists lists (cdr rest-lists))
	(list (car rest-lists) (car rest-lists))
	(last-list (only-null-list-elements-p (cdr rest-lists))
		   (only-null-list-elements-p (cdr rest-lists))))
       ((null rest-lists) strm)
    (do* ((lst list (cdr lst))
	  (elem (car lst) (car lst))
	  (last-elem (null (cdr lst)) (null (cdr lst))))
	 ((null lst))
      (write-string elem strm)
      (unless (and last-elem last-list)
	(write-string sep strm)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def-prefixed-number-string (fn-name type &optional doc)
    `(defun ,fn-name (num pchar len)
       ,@(when (stringp doc) (list doc))
       (declare (optimize (speed 3)
			  (safety 0)
			  (space 0))
		(fixnum len)
		(,type num))
       (when pchar
	 (incf len))
       (do* ((zero-code (char-code #\0))
	     (result (make-string len :initial-element #\0))
	     (minus? (minusp num))
	     (val (if minus? (- num) num)
		  (nth-value 0 (floor val 10)))
	     (pos (1- len) (1- pos))
	     (mod (mod val 10) (mod val 10)))
	    ((or (zerop val) (minusp pos))
	     (when pchar
	       (setf (schar result 0) pchar))
	     (when minus? (setf (schar result (if pchar 1 0)) #\-))
	     result)
	 (declare (,type val)
		  (fixnum mod zero-code pos)
		  #-genera (boolean minus?)
		  (simple-string result))
	 (setf (schar result pos) (code-char (the fixnum (+ zero-code mod))))))))

(def-prefixed-number-string prefixed-fixnum-string fixnum
  "Outputs a string of LEN digits with an optional initial character
PCHAR.  Leading zeros are present.  LEN must be a fixnum.")

(def-prefixed-number-string prefixed-integer-string integer
  "Outputs a string of LEN digits with an optional initial character
PCHAR.  Leading zeros are present.  LEN must be a fixnum.")

(defun integer-string (num len)
  (declare (optimize (speed 3)
		     (safety 0)
		     (space 0))
	   (type fixnum len)
	   (type integer num))
  (do* ((zero-code (char-code #\0))
	(result (make-string len :initial-element #\0))
	(minus? (minusp num))
	(val (if minus? (- 0 num) num)
	     (nth-value 0 (floor val 10)))
	(pos (1- len) (1- pos))
	(mod (mod val 10) (mod val 10)))
       ((or (zerop val) (minusp pos))
	(when minus? (setf (schar result 0) #\-))
	result)
    (declare (fixnum mod zero-code pos)
	     (simple-string result)
	     (integer val))
    (setf (schar result pos) (code-char (+ zero-code mod)))))

(defun fast-string-search (substr str substr-length startpos endpos)
  (declare (simple-string substr str)
	   (fixnum substr-length startpos endpos)
	   (optimize (speed 3)
		     (safety 0)
		     (debug 0)
		     (space 0)))
  (do* ((pos startpos (1+ pos))
	(lastpos (- endpos substr-length)))
       ((> pos lastpos) nil)
    (declare (fixnum pos lastpos))
    (do ((i 0 (1+ i)))
	((= i substr-length)
	 (return-from fast-string-search pos))
      (declare (fixnum i))
      (unless (char= (schar str (+ i pos)) (schar substr i))
	(return nil)))))

(defun string-delimited-string-to-list (str substr)
  "Splits a string delimited by substr into a list of strings."
  (declare (simple-string str substr)
	   (optimize (speed 3)
		     (safety 0)
		     (debug 0)
		     (space 0)
		     (compilation-speed 0)))
  (do* ((substr-len (length substr))
	(strlen (length str))
	(output'())
	(pos 0)
	(end (fast-string-search substr str substr-len pos strlen)
	     (fast-string-search substr str substr-len pos strlen)))
       ((null end)
	(when (< pos strlen)
	  (push (subseq str pos) output))
	(nreverse output))
    (declare (fixnum strlen substr-len pos)
	     (type (or fixnum null) end))
    (push (subseq str pos end) output)
    (setq pos (+ end substr-len))))

(defun string-to-list-skip-delimiter (str &optional (delim #\Space))
  (declare (simple-string str)
	   (optimize (speed 0)
		     (safety 0)
		     (space 0)))
  (do* ((results '())
	(end (length str))
	(i (position-not-char delim str 0 end)
	   (position-not-char delim str j end))
	(j (when i (position-char delim str i end))
	   (when i (position-char delim str i end))))
       ((or (null i) (null j))
	(when (and i (< i end))
	  (push (subseq str i end) results))
	(nreverse results))
    (declare (fixnum end)
	     (type (or fixnum null) i j))
    (push (subseq str i j) results)))

(defun string-starts-with (start str)
  (and (>= (length str) (length start))
       (string-equal start str :end2 (length start))))

(defun count-string-char (s c)
  (declare (simple-string s)
	   (character c)
	   (optimize (speed 3)
		     (safety 0)
		     (debug 0)))
  (do ((len (length s))
       (i 0 (1+ i))
       (count 0))
      ((= i len) count)
    (declare (fixnum i len count))
    (when (char= (schar s i) c)
      (incf count))))

(defun count-string-char-if (pred s)
  (declare (simple-string s)
	   (type (or function symbol) pred)
	   (optimize (speed 3)
		     (safety 0)
		     (debug 0)
		     (space 0)))
  (do ((len (length s))
       (i 0 (1+ i))
       (count 0))
      ((= i len) count)
    (declare (fixnum i len count))
    (when (funcall pred (schar s i))
      (incf count))))

(defun non-alphanumericp (ch)
  (not (alphanumericp ch)))

(defvar +hex-chars+ "0123456789ABCDEF")
(declaim (type simple-string +hex-chars+))

(defun hexchar (n)
  (declare (type (integer 0 15) n))
  (schar +hex-chars+ n))

(defconstant* +char-code-lower-a+ (char-code #\a))
(defconstant* +char-code-upper-a+ (char-code #\A))
(defconstant* +char-code-0+ (char-code #\0))
(declaim (type fixnum +char-code-0+ +char-code-upper-a+
	       +char-code-lower-a+))

(defun charhex (ch)
  (let ((code (char-code (char-upcase ch))))
    (declare (fixnum ch))
    (if (>= code +char-code-upper-a+)
	(+ 10 (- code +char-code-upper-a+))
	(- code +char-code-0+))))

(defun binary-sequence-to-hex-string (seq)
  (let ((list (etypecase seq
		(list seq)
		(sequence (map 'list #'identity seq)))))
    (string-downcase (format nil "~{~2,'0X~}" list))))

(defun encode-uri-string (query)
  (declare (simple-string query)
	   (optimize (speed 3)
		     (safety 0)
		     (space 0)
		     (debug 0)))
  (do* ((count (count-string-char-if #'non-alphanumericp query))
	(len (length query))
	(new-len (+ len (* 2 count)))
	(str (make-string new-len))
	(spos 0 (1+ spos))
	(dpos 0 (1+ dpos)))
       ((= spos len) str)
    (declare (fixnum count len new-len spos dpos)
	     (simple-string str))
    (let ((ch (schar query spos)))
      (if (non-alphanumericp ch)
	  (let ((c (char-code ch)))
	    (setf (schar str dpos) #\%)
	    (incf dpos)
	    (setf (schar str dpos) (hexchar (logand (ash c -4) 15)))
	    (incf dpos)
	    (setf (schar str dpos) (hexchar (logand c 15))))
	  (setf (schar str dpos) ch)))))

(defun decode-uri-string (query)
  (declare (simple-string query)
	   (optimize (speed 3)
		     (safety 0)
		     (debug 0)
		     (space 0)))
  (do* ((count (count-string-char query #\%))
	(len (length query))
	(new-len (- len (* 2 count)))
	(str (make-string new-len))
	(spos 0 (1+ spos))
	(dpos 0 (1+ dpos)))
       ((= spos len) str)
    (declare (fixnum count len new-len spos dpos)
	     (simple-string str))
    (let ((ch (schar query spos)))
      (if (char= #\% ch)
	  (let ((c1 (charhex (schar query (1+ spos))))
		(c2 (charhex (schar query (+ spos 2)))))
	    (declare (fixnum c1 c2))
	    (setf (schar str dpos)
		  (code-char (logior c2 (ash c1 4))))
	    (incf spos 2))
	  (setf (schar str dpos) ch)))))

(defun uri-query-to-alist (query)
  (mapcar #'(lambda (x)
	      (let ((lst (delimited-string-to-list x #\=)))
		(cons (first lst) (second lst))))
	  (delimited-string-to-list
	    (decode-uri-string query) #\&)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +unambiguous-charset+
	  "abcdefghjkmnpqrstuvwxyz123456789ABCDEFGHJKLMPQRSTUVWXYZ")
  (defconstant* +unambiguous-length+ (length +unambiguous-charset+)))

(defun random-char (&optional (set :lower-alpha))
  (ecase set
    (:lower-alpha
      (code-char (+ +char-code-lower-a+ (random 26))))
    (:lower-alphanumeric
      (let ((n (random 36)))
	(if (>= n 26)
	    (code-char (+ +char-code-0+ (- n 26)))
	    (code-char (+ +char-code-lower-a+ n)))))
    (:upper-alpha
      (code-char (+ +char-code-upper-a+ (random 26))))
    (:upper-alphanumeric
      (let ((n (random 36)))
	(if (>= n 26)
	    (code-char (+ +char-code-0+ (- n 26)))
	    (code-char (+ +char-code-upper-a+ n)))))
    (:unambiguous
      (schar +unambiguous-charset+ (random +unambiguous-length+)))
    (:upper-lower-alpha
      (let ((n (random 52)))
	(if (>= n 26)
	    (code-char (+ +char-code-upper-a+ (- n 26)))
	    (code-char (+ +char-code-lower-a+ n)))))
    (:upper-lower-alphanumeric
      (let ((n (random 62)))
	(cond ((>= n 52)
	       (code-char (+ +char-code-0+ (- n 52))))
	      ((and (>= n 26) (< n 52))
	       (code-char (+ +char-code-upper-a+ (- n 26))))
	      (t
	       (code-char (+ +char-code-lower-a+ n))))))))

(defun random-string (&key (length 10) (set :lower-alpha))
  (declare (optimize (speed 3)))
  (let ((s (make-string length)))
    (declare (simple-string s))
    (dotimes (i length s)
      (setf (char s i) (random-char set)))))

(defun first-char (s)
  (declare (simple-string s))
  (when (and (stringp s) (plusp (length s)))
    (schar s 0)))

(defun last-char (s)
  (declare (simple-string s))
  (when (stringp s)
    (let ((len (length s)))
      (when (plusp len))
      (schar s (1- len)))))

(defun ensure-string (v)
  (typecase v
    (string v)
    (character (string v))
    (symbol (symbol-name v))
    (otherwise (write-to-string v))))

(defun string-right-trim-one-char (char str)
  (declare (simple-string str)
	   (character char))
  (let* ((len (length str))
	 (last (1- len)))
    (declare (fixnum len last))
    (if (char= char (schar str last))
	(subseq str 0 last)
	str)))

(defun remove-char-string (char str)
  (declare (character char)
	   (string str))
  (do* ((len (length str))
	(out (make-string len))
	(pos 0 (1+ pos))
	(opos 0))
       ((= pos len) (subseq out 0 opos))
    (declare (fixnum pos opos len)
	     (simple-string out))
    (let ((c (char str pos)))
      (declare (character c))
      (when (char/= c char)
	(setf (schar out opos) c)
	(incf opos)))))

(defun string-strip-ending (str endings)
  (if (stringp endings)
      (setq endings (list endings)))
  (let ((len (length str)))
    (dolist (ending endings str)
      (when (and (>= len (length ending))
		 (string-equal ending (subseq str (- len (length
							   ending)))))
	(return-from string-strip-ending
	  (subseq str 0 (- len (length ending))))))))

(defun string-maybe-shorten (str maxlen)
  (string-elide str maxlen :end))

(defun string-elide (str maxlen position)
  (declare (fixnum maxlen))
  (let ((len (length str)))
    (declare (fixnum len))
    (cond ((<= len maxlen)
	   str)
	  ((<= maxlen 3)
	   "...")
	  ((eq position :middle)
	   (multiple-value-bind (mid remain)
	       (truncate maxlen 2)
	     (let ((end1 (- mid 1))
		   (start2 (- len (- mid 2) remain)))
	       (concatenate 'string (subseq str 0 end1) "..."
			    (subseq str start2)))))
	  ((or (eq position :end) t)
	   (concatenate 'string (subseq str 0 (- maxlen 3)) "...")))))

#-genera
(defun shrink-vector (str size)
  #+allegro
  (excl::.primcall 'sys::shrink-svector str size)
  #+cmu
  (lisp::shrink-vector str size)
  #+lispworks
  (system::shrink-vector$vector str size)
  #+sbcl
  (sb-kernel:shrink-vector str size)
  #+scl
  (common-lisp::shrink-vector str size)
  #-(or allegro cmu lispworks sbcl scl)
  (setq str (subseq str 0 size))
  str)

(defun lex-string (string &key (whitespace '(#\Space #\Newline)))
  (flet ((is-sep (char) (member char whitespace :test #'char=)))
    (let ((tokens nil))
      (do* ((token-start
	      (position-if-not #'is-sep string)
	      (when token-end
		(position-if-not #'is-sep string :start (1+ token-end))))
	    (token-end
	      (when token-start
		(position-if-not #'is-sep string :start token-start))
	      (when token-start
		(position-if #'is-sep string :start token-start))))
	   ((null token-start) (nreverse tokens))
	(push (subseq string token-start token-end) tokens)))))

(defun split-alphanumeric-string (string)
  (declare (simple-string string)
	   (optimize (speed 3)
		     (safety 0)
		     (debug 0)))
  (flet ((is-sep (char)
	   (declare (character char))
	   (and (non-alphanumericp char)
		(not (char= #\_ char)))))
    (let ((tokens nil))
      (do* ((token-start
	      (position-if-not #'is-sep string)
	      (when token-end
		(position-if-not #'is-sep string :start (1+ token-end))))
	    (token-end
	      (when token-start
		(position-if #'is-sep string :start token-start))
	      (when token-start
		(position-if #'is-sep string :start token-start))))
	   ((null token-start) (nreverse tokens))
	(push (subseq string token-start token-end) tokens)))))

(defun trim-non-alphanumeric (word)
  (declare (simple-string word)
	   (optimize (speed 3)
		     (safety 0)
		     (debug 0)
		     (space 0)))
  (let* ((start 0)
	 (len (length word))
	 (end len))
    (declare (fixnum start len end))
    (do ((done nil))
	((or done (= start end)))
      (if (alphanumericp (schar word start))
	  (setq done t)
	  (incf start)))
    (when (> end start)
      (do ((done nil))
	  ((or done (= start end)))
	(if (alphanumericp (schar word (1- end)))
	    (setq done t)
	    (decf end))))
    (if (or (plusp start) (/= len end))
	(subseq word start end)
	word)))

(defun collapse-whitespace (s)
  (declare (simple-string s)
	   (optimize (speed 3)
		     (safety 0)
		     (debug 0)
		     (space 0)))
  (with-output-to-string (stream)
    (do ((pos 0 (1+ pos))
	 (in-white nil)
	 (len (length s)))
	((= pos len))
      (declare (fixnum pos len))
      (let ((c (schar s pos)))
	(declare (character c))
	(cond ((is-char-whitespace c)
	       (unless in-white
		 (write-char #\Space stream))
	       (setq in-white t))
	      (t
	       (setq in-white nil)
	       (write-char c stream)))))))

(defun string->list (string)
  (let ((eof (list nil)))
    (with-input-from-string (stream string)
      (do ((x (read stream nil eof) (read stream nil eof))
	   (l nil (cons x l)))
	  ((eq x eof) (nreverse l))))))

(defun reverse-string (string)
  (concatenate 'string (reverse (concatenate 'list string nil))))

(defun strip-non-alphanumeric (string &optional (kill-whitespace nil))
  (collapse-whitespace
    (string-trim-whitespace
      (concatenate 'string (loop for i in (concatenate 'list string)
				 when (or (alphanumericp i)
					  (and (not kill-whitespace)
					       (is-char-whitespace i)))
				   collect i)))))

(defun string-palindromic-p (string)
  (let ((str-forward (strip-non-alphanumeric (string-upcase string)))
	(str-backward (reverse-string
		       (strip-non-alphanumeric (string-upcase string)))))
    (string= str-forward str-backward)))

(defun strip-newlines (string)
  (let ((len (length string)))
    (if (char= (schar string (1- len)) #\NewLine)
	(setf (schar string (1- len)) #\Null))
    string))

;; strings.lisp ends here
