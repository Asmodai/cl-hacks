;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-UUID; Base: 10; Lowercase: Yes -*-
;;;

;;;
;;; A simple Genera port of Boian Tzonev's UUID package.
;;;
;;;
;;; TODO: Support generation of v3 and v5 UUID.
;;;

#-genera
(error "Please do not use this.  Use a proper UUID package instead.")

(defvar *clock-seq* 0
  "Holds the clock sequence.  It is set when a version 1 UUID is
generated for the first time, and remains unchanged during a whole
session.")

(defvar *node* nil
  "Holds the CHAOS number for this Lisp Machine.")

(defvar *ticks-per-count* 1024
  "Holds the amount of ticks per count.  The ticks per count determine
the number of possible version 1 UUIDs created for one time interval.")

(defclass uuid ()
    ((time-low
       :initarg :time-low
       :initform 0
       :accessor time-low)
     (time-mid
       :initarg :time-mid
       :initform 0
       :accessor time-mid)
     (time-high-and-version
       :initarg :time-high
       :initform 0
       :accessor time-high)
     (clock-seq-and-reserved
       :initarg :clock-seq-var
       :initform 0
       :accessor clock-seq-var)
     (clock-seq-low
       :initarg :clock-seq-low
       :initform 0
       :accessor clock-seq-low)
     (node
       :initarg :node
       :initform 0
       :accessor node))
  (:documentation "Represents a UUID."))

(defun make-uuid-from-string (uuid-string)
  "Creates a UUID from the string representation of a UUID."
  (make-instance
    'uuid
    :time-low (parse-integer uuid-string :start 0 :end 8 :radix 16)
    :time-mid (parse-integer uuid-string :start 9 :end 13 :radix 16)
    :time-high (parse-integer uuid-string :start 14 :end 18 :radix 16)
    :clock-seq-var (parse-integer uuid-string
                                  :start 14
                                  :end 18
                                  :radix 16)
    :clock-seq-low (parse-integer uuid-string
                                  :start 21
                                  :end 23
                                  :radix 16)
    :node (parse-integer uuid-string :start 24 :end 36 :radix 16)))

(defparameter +namespace-dns+
              (make-uuid-from-string
                "6ba7b810-9dad-11d1-80b4-00c04fd430c8"))

(defparameter +namespace-url+
              (make-uuid-from-string
                "6ba7b811-9dad-11d1-80b4-00c04fd430c8"))

(defparameter +namespace-oid+
              (make-uuid-from-string
                "06a7b821-9dad-11d1-80b4-00c04fd430c8"))

(defparameter +namespace-x500+
              (make-uuid-from-string
                "6ba7b814-9dad-11d1-80b4-00c04fd430c8"))

#+3600
(defparameter *board-info*
              '((170018 :chassis 1 "3600")
                (170027 :chassis 2 "3670")
                (120219 :chassis 3 "3640")
                (364202 :chassis 4 "3650")
                (365450 :chassis 5 "3620")
                (170072 :io 6 "IO REV 2A")
                (170157 :io 7 "IO REV 6")
                (170495 :io 8 "NBS FEP I/O")
                (170423 :io 9 "NBS FEP I/O (NEC)")))

;;; Based on code from Symbolics Statice.
(defun get-machine-serial-number ()
  #+3600
  (let ((chasis-serial-number nil)
        (io-serial-number nil))
    (dolist (board (si:get-machine-configuration))
      (let ((info (assoc (getf board :part-number) *board-info*)))
        (when info
          (case (second info)
            (:chassis
              (when chassis-serial-number
                (error "Error examining machine configuration~@
                        information.  Apparently there are two~@
                        chassis boards!"))
              (let ((num (getf board :serial-number)))
                (unless (zerop num)
                  (setq chassis-serial-number
                        (dpb (third info) (byte 4 20) num)))))
            (:io
              (when io-serial-number
                (error "Error examining machine configuration~@
                        information.  Apparently there are two~@
                        IO boards!"))
              (let ((num (getf board :serial-number)))
                (unless (zerop num)
                  (setq io-serial-number
                        (dpb (third info) (byte 4 20) num)))))))))
    (or chassis-serial-number
        io-serial-number
        (error "Error examining machine configuration.  Apparently~@
                there is neither a valid chassis nor a valid IO!~@
                Valid means it exists and has a non-zero serial~@
                number.")))
  #+(or VLM IMACH)
  (multiple-value-bind (nil serial-number)
      (si:machine-model)
    serial-number)
  #-(or 3600 VLM IMACH)
  (error "What? I'm meant to do something with ~A on an ~A?"
         (lisp-implementation-type)
         (machine-type)))

(defun get-node-id ()
  (let ((node (get-machine-serial-number)))
    (when (not node)
      (setf node (dpb #b01 (byte 8 0)
                      (random #xffffffffffff))))
    node))

(defun load-bytes (b-array &key (byte-size 8) (start 0) end)
  "Helper function to load bytes from a byte array, returning them as an
integer."
  (let ((ret-val 0))
    (loop for i from start to end
          for pos from (- end start) downto 0
          do (setf ret-val
                   (dpb (aref b-array i)
                        (byte byte-size (* pos byte-size))
                        ret-val)))
    ret-val))

(let ((uuids-this-tick 0)
      (last-time 0))
  (defun get-timestamp ()
    "Get timestamp, compensate nanosecond intervals."
    (tagbody
      restart
         (let ((time-now (+ (* (get-universal-time) 10000000)
                            100103040000000000)))
           (cond ((not (= last-time time-now))
                  (setf uuids-this-tick 0
                        last-time time-now)
                  (return-from get-timestamp time-now))
                 (t
                  (cond ((< uuids-this-tick *ticks-per-count*)
                         (incf uuids-this-tick)
                         (return-from get-timestamp
                           (+ time-now uuids-this-tick)))
                        (t
                         (sleep 0.0001)
                         (go restart)))))))))

(defun format-v3or5-uuid (hash ver)
  "Helper function to format a version 3 or 5 UUID."
  (when (or (= ver 3)
            (= ver 5))
    (make-instance
      'uuid
      :time-low (load-bytes hash :start 0 :end 3)
      :time-mid (load-bytes hash :start 4 :end 5)
      :time-high (cond ((= ver 3)
                        (dpb #b0011 (byte 4 12)
                             (load-bytes hash :start 6 :end 7)))
                       ((= ver 5)
                        (dpb #b0101 (byte 4 12)
                             (load-bytes hash :start 6 :end 7))))
      :clock-seq-var (dpb #b10 (byte 2 6) (aref hash 8))
      :clock-seq-low (aref hash 9)
      :node (load-bytes hash :start 10 :end 15))))

(defgeneric print-uuid (id &optional stream)
  (:method ((id uuid) &optional (stream *standard-output*))
   (format stream "~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~12,'0X"
           (time-low id)
           (time-mid id)
           (time-high id)
           (clock-seq-var id)
           (clock-seq-low id)
           (node id))))

(defmethod print-object ((id uuid) stream)
  (print-unreadable-object (id stream :identity nil :type t)
    (print-uuid id stream)))

(defun print-bytes (stream uuid)
  (format stream "~8,'0X~4,'0X~4,'0X~2,'0X~12,'0X"
          (time-low uuid)
          (time-mid uuid)
          (time-high uuid)
          (clock-seq-var uuid)
          (clock-seq-low uuid)
          (node uuid)))

(defun format-as-urn (stream uuid)
  (format stream "urn:uuid:~(~A~)" uuid))

(defun make-null-uuid ()
  (make-instance 'uuid))

(defun make-v1-uuid ()
  (let ((timestamp (get-timestamp)))
    (when (zerop *clock-seq*)
      (setf *clock-seq* (random 10000)))
    (unless *node*
      (setf *node* (get-node-id)))
    (make-instance
      'uuid
      :time-low (ldb (byte 32 0) timestamp)
      :time-mid (ldb (byte 16 32) timestamp)
      :time-high (dpb #b0001 (byte 4 12) 
                      (ldb (byte 12 48) timestamp))
      :clock-seq-var (dpb #b10 (byte 2 6)
                          (ldb (byte 6 8) *clock-seq*))
      :clock-seq-low (ldb (byte 8 0) *clock-seq*)
      :node *node*)))

(defun make-v3-uuid (namespace name)
  (declare (ignore namespace name))
  (error "UUID v3 isn't implemented on Genera yet."))

(defun make-v4-uuid ()
  (make-instance
    'uuid
    :time-low (random #xffffffff)
    :time-mid (random #xffff)
    :time-high (dpb #b0100 (byte 4 12)
                    (ldb (byte 12 0) (random #xffff)))
    :clock-seq-var (dpb #b10 (byte 2 6)
                        (ldb (byte 8 0) (random #xff)))
    :clock-seq-low (random #xff)
    :node (random #xffffffffffff)))

(defun make-v5-uuid (namespace name)
  (declare (ignore namespace name))
  (error "UUID v5 isn't supported on Genera yet."))

(defun uuid-to-byte-array (uuid)
  (let ((array (make-array 16 :element-type '(unsigned-byte 8))))
    (with-slots (time-low time-mid time-high-and-version
                 clock-seq-and-reserved clock-seq-low node)
      uuid
      (loop for i from 3 downto 0
            do (setf (aref array (- 3 i))
                     (ldb (byte 8 (* 8 i)) time-low)))
      (loop for i from 5 downto 4
            do (setf (aref array i)
                     (ldb (byte 8 (* 8 (- 5 i))) time-mid)))
      (loop for i from 7 downto 6
            do (setf (aref array i)
                     (ldb (byte 8 (* 8 (- 7 i)))
                          time-high-and-version)))
      (setf (aref array 8) (ldb (byte 8 0) clock-seq-and-reserved)
            (aref array 9) (ldb (byte 8 0) clock-seq-low))
      (loop for i from 15 downto 10
            do (setf (aref array i)
                     (ldb (byte 8 (* 8 (- 15 i))) node)))
      array)))

(defmacro arr-to-bytes (from to array)
  `(loop for i from ,from to ,to
         with res = 0
         do (setf (ldb (byte 8 (* 8 (- ,to i))) res)
                  (aref ,array i))
         finally (return res)))

(defun byte-array-to-uuid (array)
  (assert (and (= (array-rank array) 1)
               (= (array-total-size array) 16))
          (array)
          "Please provide a one-dimensional array with 16 elements~@
           of type (unsigned-byte 8)")
  (make-instance
    'uuid
    :time-low (arr-to-bytes 0 3 array)
    :time-mid (arr-to-bytes 4 5 array)
    :time-high (arr-to-bytes 6 7 array)
    :clock-seq-var (aref array 8)
    :clock-seq-low (aref array 9)
    :node (arr-to-bytes 10 15 array)))

(defun digest-uuid (ver uuid name)
  (declare (ignore ver uuid name))
  (error "This function is not implemented on Genera."))

(defun get-bytes (uuid-string)
  (with-output-to-string (out)
    (loop for i = 0 then (+ i 1)
          as j = (+ i 2)
          with max = (- (length uuid-string) 2)
          as cur-pos = (parse-integer (subseq uuid-string i j)
                                      :radix 16)
          do (format out "~A" (code-char cur-pos))
          while (< i max))
    out))

;;; EOF
