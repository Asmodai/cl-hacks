;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-HACKS-INTERNALS; Base: 10; Lowercase: Yes -*-
;;;
;;; documentation.lisp --- Documentation strings.
;;;
;;; Time-stamp: <Sunday Feb  5, 2012 01:49:36 asmodai>
;;; Revision:   21
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Nov 2011 15:53:12
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

#+genera
(error "You shouldn't be loading this file, after all pretty much ~
        all the documentation will (should!) exist as Concordia ~
        documents.")

#-genera
(in-package #:cl-hacks-internals)

;;; ==================================================================
;;;{{{ Documentation function:

(defmacro define-documentation (name type doc)
  (cond ((eq type 'function)
         `(when (fboundp ,name)
            (setf (documentation ,name ,type) ,doc)))
        ((eq type 'variable)
         `(when (boundp ,name)
            (setf (documentation ,name ,type) ,doc)))
        (t
         ;; TODO: Add checking for other documentation types here
         ;; please, e.g. slots, classes, packages, et al.
         `(ignore-errors
            (setf (documentation ,name ,type) ,doc)))))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Doc strings:

;;; ------------------------------------------------------------------
;;;{{{ anaphoric.lisp:

(define-documentation 'aif 'function
  "An anaphoric version of `if' that allows the 'then-form' and
'else-form' forms to refer back to the object being tested.

The thing being tested by the conditional is bound to the symbol 'it'
within the scope of the conditional.

As an example let us consider the following:

   (let ((it (big-long-calculation))
     (if it
         (foo it)
         nil))

We can use `aif' to write it thusly:

   (aif (big-long-calculation)
        (foo it)
        nil))

The `aif' macro is defined on page 190 of 'On Lisp' by Paul Graham.")

(define-documentation 'awhen 'function
  "An anaphoric version of `when' that allows the body to refer back
to the object being tested.

The thing being tested by the conditional is bound to the symbol 'it'
within the scope of the conditional.")

(define-documentation 'awhile 'function
  "An anaphoric version of `while' that allows the body to refer back
to the object being tested.

The thing being tested by the conditional is bound to the symbol 'it'
within the scope of the conditional.")

(define-documentation 'aand 'function
  "An anaphoric version of `and' that allows the body to refer back to
the object being tested.

The thing being tested by the conditional is bound to the symbol 'it'
within the scope of the conditional.")

(define-documentation 'acond 'function
  "An anaphoric version of `cond' that allows the body to refer back
to the object being tested.

The thing being tested by the conditional is bound to the symbol 'it'
within the scope of the conditional.")

(define-documentation 'alambda 'function
  "An anaphoric version of `lambda' that allows the body of the lambda
to refer to itself by binding it to the label 'self'.

An example of an anamorphic lambda would look thus:

   (setq foo '(1 2 3 4 5 6 7 8)) => (1 2 3 4 5 6 7 8)
   (mapcar (alambda (x)
     (if (evenp x)
                 x
                 (self (1+ x))))
   foo)
=> (2 2 4 4 6 6 8 8)")

(define-documentation 'ablock 'function
  "An anaphoric version of `block' that allows the body of the block
to refer to itself by using `alambda'.

A trivial example of `ablock' would be:

   (ablock north-pole
     (princ HO)
     (princ it)
     (princ it)
     (return-from north-pole))")

(define-documentation 'aif2 'function
  "Similar to `aif' but rather than binding and testing the same
value, binds the first but tests the second.

As an example, take the following:

   (defun edible? (x)
     (multiple-value-bind (val found?)
 (gethash x edible)
       (if found?
   (if val 'yes 'no)
   'maybe)))

The test is performed on the second value from `gethash' rather than
on the value returned.  `aif2' allows us utilize this form of testing
for failure as such:

   (defun edible? (x)
     (aif2 (gethash x edible)
   (if it 'yes 'no)
   'maybe))")

(define-documentation  'awhen2 'function
  "Similar to `awhen', but tests on the failure value of the test form
rather than the return value.")

(define-documentation 'awhile2 'function
  "Similar to `awhile', but tests on the failure value of the test
form rather than the return value.")

(define-documentation 'acond2 'function
  "Similar to `acond', but tests on the failure value of the clauses
rather than any return values.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ arrays.lisp:

(define-documentation 'copy-array 'function
  "Returns an undisplaced copy of ARRAY, with same fill-pointer
and adjustability (if any) as the original, unless overridden by
the keyword arguments.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ bindings.lisp:

(define-documentation 'if-let 'function
  "Creates new variable bindings, and conditionally executes either
THEN-FORM or ELSE-FORM. ELSE-FORM defaults to NIL.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified
order. Then all the variables are bound to the corresponding values.

If all variables were bound to true values, the THEN-FORM is executed
with the bindings in effect, otherwise the ELSE-FORM is executed with
the bindings in effect.")

(define-documentation 'when-let 'function
  "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified
order. Then all the variables are bound to the corresponding values.

If all variables were bound to true values, then FORMS are executed as
an implicit PROGN.")

(define-documentation 'when-let* 'function
  "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

Each initial-form is executed in turn, and the variable bound to the
corresponding value. Initial-form expressions can refer to variables
previously bound by the WHEN-LET*.

Execution of WHEN-LET* stops immediately if any initial-form evaluates
to NIL.

If all initial-forms evaluate to true, then FORMS are executed as an
implicit PROGN.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ conditions.lisp:

(define-documentation 'required-argument 'function
  "Signals an error for a missing argument of NAME.

Intended for use as an initialisation form for structure and class
slots, and a default value for required keyword arguments.")

(define-documentation 'ignore-some-conditions 'function
  "Similar to CL:IGNORE-ERRORS, but the (unevaluated) CONDITIONS list
determines which specific conditions are to be ignored.")

(define-documentation 'unwind-protect-case 'function
  "Like CL:UNWIND-PROTECT, but you can specify the circumstances that
the cleanup CLAUSES are run.

ABORT-FLAG is the name of a variable that will be bound to T in
CLAUSES if the PROTECTED-FORM aborted preemptively, and to NIL
otherwise.

Examples:

  (unwind-protect-case ()
       (protected-form)
     (:normal (format t ``PROTECTED-FORM executed normally.''))
     (:abort (format t ``PROTECTED-FORM aborted.''))
     (:always (format t ``All other cases.'')))

  (unwind-protect-case (aborted-p)
       (protected-form)
     (:always (perform-cleanup-if aborted-p)))")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ control-flow.lisp:

(define-documentation 'extract-function-name 'function
  "Useful for macros that want to mimic the functional interface for
functions like #'eq and 'eq.")

(define-documentation 'switch 'function
  "Evaluates first matching clause, returning its values, or evaluates
and returns the values of DEFAULT if no keys match.")

(define-documentation 'eswitch 'function
  "Like SWITCH, but signals an error if no key matches.")

(define-documentation 'cswitch 'function
  "Like SWITCH, but signals a continuable error if no key matches.")

(define-documentation 'whichever 'function
  "Evaluates exactly one of POSSIBILITIES, chosen at random.")

(define-documentation 'xor 'function
  "Evaluates its argument one at a time, from left to right. If more
then one argument evaluates to a true value no further DATUMS are
evaluated, and NIL is returned as both primary and secondary value. If
exactly one argument evaluates to true, its value is returned as the
primary value after all the arguments have been evaluated, and T is
returned as the secondary value. If no arguments evaluate to true
NIL is retuned as primary, and T as secondary value.")

(define-documentation 'nth-value-or 'function
  "Evaluates FORM arguments one at a time, until the NTH-VALUE
returned by one of the forms is non-NIL. It then returns all the
values returned by evaluating that form. If none of the forms return a
non-nil nth value, this form returns  NIL.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ definitions.lisp:

(define-documentation 'define-constant 'function
  "Ensures that the global variable named by NAME is a constant with a
value that is equal under TEST to the result of evaluating
INITIAL-VALUE. TEST is a /function designator/ that defaults to
EQL. If DOCUMENTATION is given, it becomes the documentation string of
the constant.

Signals an error if NAME is already a bound non-constant variable.

Signals an error if NAME is already a constant variable whose value is
not equal under TEST to result of evaluating INITIAL-VALUE.")

(define-documentation 'defconstant* 'function
  "Like DEFCONSTANT but ensure VALUE is evaluated only once.")

(define-documentation 'defcustom 'function
  "Define a typed global constant.")

(define-documentation 'defconst 'function
  "Define a typed contant or variable, depending on the subtype of the
given TYPE.")

(define-documentation 'make-typed-array 'function
  "Make an array with elements of TYPE, with initialization.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ emacs.lisp:

(define-documentation 'define-indentation 'function
  "Defines an indentation rule for an editor.

PAIRS contains a symbol and either another symbol or a list.

The symbol in the first element of the pair is the symbol to define an
indentation rule for, and the second symbol or list of the pair is
either a symbol to copy an indentation rule from or a list that
defines an intendation rule.

Indentation rule lists are in the form of:

   (no-of-sexps-to-skip amount-to-change-indentation)

Right now, DEFINE-INDENTATION supports ZWEI, LispWorks, and Franz
Allegro Common Lisp.")

(define-documentation 'describe-indentation 'function
  "Describes any indentation rules defined for the given symbols to
standard output.

This macro has no use other than confirming the indentation for the
given symbols.

Right now, DESCRIBE-INDENTATION supports ZWEI, LispWorks, and Franz
Allegro Common Lisp.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ features.lisp:

(define-documentation 'featurep 'function
  "Returns T if the argument matches the state of the *FEATURES* 
list; otherwise NIL is returned.  FEATURE-EXPR can be any atom or
list acceptable to the reader macros #+ and #-.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ functions.lisp:

(define-documentation 'ensure-function 'function
  "Returns the function designated by FUNCTION-DESIGNATOR: if
FUNCTION-DESIGNATOR is a function, it is returned, otherwise it must
be a function name and its FDEFINITION is returned.")

(define-documentation 'disjoin 'function
  "Returns a function that applies each of PREDICATE and
MORE-PREDICATE functions in turn to its arguments, returning the
primary value of the first predicate that returns true, without
calling the remaining predicates. If none of the predicates returns
true, NIL is returned.")

(define-documentation 'conjoin 'function
  "Returns a function that applies each of PREDICATE and
MORE-PREDICATE functions in turn to its arguments, returning NIL if
any of the predicates returns false, without calling the remaining
predicates. If none of the predicates returns false, returns the
primary value of the last predicate.")

(define-documentation 'compose 'function
  "Returns a function composed of FUNCTION and MORE-FUNCTIONS that
applies its arguments to to each in turn, starting from the rightmost
of MORE-FUNCTIONS, and then calling the next one with the primary
value of the last.")

(define-documentation 'multiple-value-compose 'function
  "Returns a function composed of FUNCTION and MORE-FUNCTIONS that
applies its arguments to to each in turn, starting from the rightmost
of MORE-FUNCTIONS, and then calling the next one with all the return
values of the last.")

(define-documentation 'curry 'function
  "Returns a function that applies ARGUMENTS and the arguments it is
called with to FUNCTION.")

(define-documentation 'rcurry 'function
  "Returns a function that applies the arguments it is called with and
ARGUMENTS to FUNCTION.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ hash-tables.lisp:

(define-documentation 'copy-hash-table 'function
  "Returns a copy of hash table TABLE, with the same keys and values
as the TABLE. The copy has the same properties as the original,
unless overridden by the keyword arguments.

Before each of the original values is set into the new hash-table, KEY
is invoked on the value. As KEY defaults to CL:IDENTITY, a shallow
copy is returned by default.")

(define-documentation 'maphash-keys 'function
  "Like MAPHASH, but calls FUNCTION with each key in the hash table
TABLE.")

(define-documentation 'maphash-values 'function
  "Like MAPHASH, but calls FUNCTION with each value in the hash table
TABLE.")

(define-documentation 'hash-table-keys 'function
  "Returns a list containing the keys of hash table TABLE.")

(define-documentation 'hash-table-values 'function
  "Returns a list containing the values of hash table TABLE.")

(define-documentation 'hash-table-alist 'function
  "Returns an association list containing the keys and values of hash
table TABLE.")

(define-documentation 'hash-table-plist 'function
  "Returns a property list containing the keys and values of hash
table TABLE.")

(define-documentation 'alist-hash-table 'function
  "Returns a hash table containing the keys and values of the
association list ALIST. Hash table is initialized using the
HASH-TABLE-INITARGS.")

(define-documentation 'plist-hash-table 'function
  "Returns a hash table containing the keys and values of the property
list PLIST. Hash table is initialized using the HASH-TABLE-INITARGS.")

(define-documentation 'ensure-gethash 'function
  "Like GETHASH, but if KEY is not found in the HASH-TABLE saves the
DEFAULT under key before returning it. Secondary return value is true
if key was already in the table.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ ifstar.lisp:

(define-documentation 'if* 'function
  "This form consists of a series of clauses introduced by the symbols
then, elseif, else, and thenret. First the predicate test-form is
evaluated. If it is true, the then-forms are evaluated, and the
value of the last such form is returned. If test-form evaluates to
nil, any remaining clauses are processed. If no clauses remain, if*
returns nil.

When a thenret clause is encountered no further evaluation takes
place, and the value of the most recently evaluated test-form is
returned.

When an elseif clause is encountered, the predicate else-test-form is
evaluated. If it is true, the else-then-forms are evaluated, and the
value of the last such form is returned; otherwise any remaining
clauses are processed. If no clauses remain, if* returns nil. And
lastly, when an else clause is encountered, the else-forms are
evaluated, and the value of the last such form is returned.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ lists.lisp:

(define-documentation 'alist-plist 'function
  "Returns a property list containing the same keys and values as the
association list ALIST in the same order.")

(define-documentation 'plist-alist 'function
  "Returns an association list containing the same keys and values as
the property list PLIST in the same order.")

(define-documentation 'malformed-plist 'function
  "Generates an error informing the user of a malformed plist.")

(define-documentation 'doplist 'function
  "Iterates over elements of PLIST. BODY can be preceded by
declarations, and is like a TAGBODY. RETURN may be used to terminate
the iteration early. If RETURN is not used, returns VALUES.")

(define-documentation 'circular-list 'function
  "Creates a circular list of ELEMENTS.")

(define-documentation 'list-to-circular-list 'function
  "Generate a circular list from the given list.")

(define-documentation 'circular-list-p 'function
  "Returns true if OBJECT is a circular list, NIL otherwise.")

(define-documentation 'circular-tree-p 'function
  "Returns true if OBJECT is a circular tree, NIL otherwise.")

(define-documentation 'proper-list-p 'function
  "Returns true if OBJECT is a proper list.")

(define-documentation 'proper-list 'type
  "Type designator for proper lists. Implemented as a SATISFIES type,
hence not recommended for performance intensive use. Main usefullness
as a type designator of the expected type in a TYPE-ERROR.")

(define-documentation 'lastcar 'function
  "Returns the last element of LIST. Signals a type-error if LIST is
not a proper list.")

(define-documentation 'make-circular-list 'function
  "Creates a circular list of LENGTH with the given INITIAL-ELEMENT.")

(define-documentation 'circular-list 'type
  "Type designator for circular lists. Implemented as a SATISFIES
type, so not recommended for performance intensive use. Main
usefullness as the expected-type designator of a TYPE-ERROR.")

(define-documentation 'ensure-car 'function
  "If THING is a CONS, its CAR is returned. Otherwise THING is
returned.")

(define-documentation 'ensure-cons 'function
   "If CONS is a cons, it is returned. Otherwise returns a fresh cons
with CONS in the car, and NIL in the cdr.")

(define-documentation 'ensure-list 'function
  "If LIST is a list, it is returned. Otherwise returns the list
designated by LIST.")

(define-documentation 'remove-from-plist 'function
  "Returns a propery-list with same keys and values as PLIST, except
that keys in the list designated by KEYS and values corresponding to
them are removed. The returned property-list may share structure with
the PLIST, but PLIST is not destructively modified. Keys are compared
using EQ.")

(define-documentation 'delete-from-plist 'function
  "Just like REMOVE-FROM-PLIST, but this version may destructively
modify the provided plist.")

(define-documentation 'sans 'function
  "Alias of REMOVE-FROM-PLIST for backward compatibility.")

(define-documentation 'mappend 'function
  "Applies FUNCTION to respective element(s) of each LIST, appending
all the all the result list to a single list. FUNCTION must return a
list.")

(define-documentation 'setp 'function
  "Returns true if OBJECT is a list that denotes a set, NIL
otherwise. A list denotes a set if each element of the list is unique
under KEY and TEST.")

(define-documentation 'set-equal 'function
  "Returns true if every element of LIST1 matches some element of
LIST2 and every element of LIST2 matches some element of
LIST1. Otherwise returns false.")

(define-documentation 'map-product 'function
  "Returns a list containing the results of calling FUNCTION with one
argument from LIST, and one from each of MORE-LISTS for each
combination of arguments.

In other words, returns the product of LIST and MORE-LISTS using
FUNCTION.

Example:

 (map-product 'list '(1 2) '(3 4) '(5 6))
  => ((1 3 5) (1 3 6) (1 4 5) (1 4 6)
      (2 3 5) (2 3 6) (2 4 5) (2 4 6))")

(define-documentation 'flatten 'function
  "Traverses the tree in order, collecting non-null leaves into a
list.")

(define-documentation 'push-end 'function
  "This is similar to PUSH except that it puts the new element at the
end of the existing list.  This preserves the order of the elements as
they are added to the list.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ looping.lisp:

(define-documentation 'til 'function
  "Evaluate the BODY form until TEST-FORM evaluates to T.")

(define-documentation 'while 'function
  "Evaluate the BODY form while TEST-FORM does not evaluate to T.")

(define-documentation 'loop-for 'function
  "Loop over VAR from START until STOP, evaluating BODY upon each
 iteration.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ macros.lisp:

(define-documentation 'with-gensyms 'function
  "Binds each variable named by a symbol in NAMES to a unique symbol
around FORMS. Each of NAMES must either be either a symbol, or of
the form:

  (symbol string-designator)

Bare symbols appearing in NAMES are equivalent to:

  (symbol symbol)

The string-designator is used as the argument to GENSYM when
constructing the unique symbol the named variable will be bound to.")

(define-documentation 'with-unique-names 'function
  "Alias for WITH-GENSYMS.")

(define-documentation 'once-only 'function
  "Each SPEC must be either a NAME, or a (NAME INITFORM), with plain
NAME using the named variable as initform.

Evaluates FORMS with names rebound to temporary variables, ensuring
that each is evaluated only once.

Example:
  (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
  (let ((y 0)) (cons1 (incf y))) => (1 . 1)")

(define-documentation 'with-ignore-errors 'function
  "Evaluate each form in FORMS within the context of IGNORE-ERRORS.")

(define-documentation 'discard-docstring 'function
  "Discards any documentation strings from BODY-VAR.")

(define-documentation 'parse-body 'function
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given.")

(define-documentation 'parse-ordinary-lambda-list 'function
  "Parses an ordinary lambda-list, returning as multiple values:

 1. Required parameters.
 2. Optional parameter specifications, normalized into form (NAME INIT
    SUPPLIEDP) where SUPPLIEDP is NIL if not present.
 3. Name of the rest parameter, or NIL.
 4. Keyword parameter specifications, normalized into form
    ((KEYWORD-NAME NAME) INIT SUPPLIEDP) where SUPPLIEDP is NIL if not
    present.
 5. Boolean indicating &ALLOW-OTHER-KEYS presence.
 6. &AUX parameter specifications, normalized into form (NAME INIT).

Signals a PROGRAM-ERROR is the lambda-list is malformed.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ sequences.lisp:

(define-documentation 'rotate 'function
  "Returns a sequence of the same type as SEQUENCE, with the elements
of SEQUENCE rotated by N: N elements are moved from the end of the
sequence to the front if N is positive, and -N elements moved from the
front to the end if N is negative.

SEQUENCE must be a proper sequence. N must be an integer, defaulting
to 1. If absolute value of N is greater then the length of the
sequence, the results are identical to calling ROTATE with
(* (SIGNUM N) (MOD N (LENGTH SEQUENCE))).

The original sequence may be destructively altered, and result
sequence may share structure with it.")

(define-documentation 'shuffle 'function
  "Returns a random permutation of SEQUENCE bounded by START and
END. Permuted sequence may share storage with the original
one. Signals an error if SEQUENCE is not a proper sequence.")

(define-documentation 'random-elt 'function
  "Returns a random element from SEQUENCE bounded by START and
END. Signals an error if the SEQUENCE is not a proper sequence.")

(define-documentation 'proper-sequence 'type
  "Type designator for proper sequences, that is proper lists and
sequences that are not lists.")

(define-documentation 'emptyp 'function
  "Returns true if SEQUENCE is an empty sequence. Signals an error if
SEQUENCE is not a sequence")

(define-documentation 'length= 'function
  "Takes any number of sequences or integers in any order. Returns
true iff the length of all the sequences and the integers are
equal.

Hint: there's a compiler macro that expands into more efficient
code if the first argument is a literal integer.")

(define-documentation 'sequence-of-length-p 'function
  "Return true if SEQUENCE is a sequence of length LENGTH. Signals an
error if SEQUENCE is not a sequence. Returns FALSE for circular
lists.")

(define-documentation 'copy-sequence 'function
  "Returns a fresh sequence of TYPE, which has the same elements as
SEQUENCE.")

(define-documentation 'first-elt 'function
  "Returns the first element of SEQUENCE. Signals a type-error if
SEQUENCE is not a sequence, or is an empty sequence.")

(define-documentation 'last-elt 'function
  "Returns the last element of SEQUENCE. Signals a type-error if
SEQUENCE is not a proper sequence, or is an empty sequence.")

(define-documentation 'starts-with-subseq 'function
   "Test whether the first elements of SEQUENCE are the same (as per
TEST) as the elements of PREFIX.

If RETURN-SUFFIX is T the functions returns, as a second value, a
displaced array pointing to the sequence after PREFIX.")

(define-documentation 'ends-with-subseq 'function
  "Test whether SEQUENCE ends with SUFFIX. In other words: return true
if the last (length SUFFIX) elements of SEQUENCE are equal to
SUFFIX.")

(define-documentation 'starts-with 'function
  "Returns true if SEQUENCE is a sequence whose first element is EQL
to OBJECT.

Returns NIL if the SEQUENCE is not a sequence or is an
empty sequence.")

(define-documentation 'ends-with 'function
  "Returns true if SEQUENCE is a sequence whose last element is EQL to
OBJECT.
Returns NIL if the SEQUENCE is not a sequence or is an empty sequence.
Signals an error if SEQUENCE is an improper list.")

(define-documentation 'map-combinations 'function
  "Calls FUNCTION with each combination of LENGTH constructable from
the elements of the subsequence of SEQUENCE delimited by START and
END.

START defaults to 0, END to length of SEQUENCE, and LENGTH to the
length of the delimited subsequence. (So unless LENGTH is specified
there is only a single combination, which has the same elements as the
delimited subsequence.)

If COPY is true (the default) each combination is freshly
allocated. If COPY is false all combinations are EQ to each other, in
which case consequences are specified if a combination is modified by
FUNCTION.")

(define-documentation 'map-permutations 'function
  "Calls function with each permutation of LENGTH constructable from
the subsequence of SEQUENCE delimited by START and END. START defaults
to 0, END to length of the sequence, and LENGTH to the length of the
delimited subsequence.")

(define-documentation 'map-derangements 'function
  "Calls FUNCTION with each derangement of the subsequence of SEQUENCE
denoted by the bounding index designators START and END. Derangement
is a permutation of the sequence where no element remains in
place. SEQUENCE is not modified, but individual derangements are EQ to
each other. Consequences are unspecified if calling FUNCTION modifies
either the derangement or SEQUENCE.")

(define-documentation 'nsubseq 'function
  "A destructive version of SUBSEQ.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ symbolics.lisp:

(define-documentation 'named-lambda 'function
  "NAMED-LAMBDA is a Zetalisp compatability macro.  It is synomymous
with LAMBDA.")

(define-documentation 'rest1 'function
  "Returns the rest of the elements of a list, starting with element 1
(counting the first element as the zeroth).  Thus, REST1 is equivalent
to CDR; the reason this function is provided is that it makes more
sense when you are thinking of the argument as a list rather than
just a cons.")

(define-documentation 'mem 'function
  "Returns NIL if ITEM is not one of the elements of LIST.  Otherwise,
it returns the sublist of LIST beginning with the first occurrence
of ITEM; that is, it returns the first cons of the list whose car is
ITEM.  The comparison is made by PRED.  Because MEM returns NIL if it
does not find anything, and something non-NIL if it finds something,
it is often used as a predicate.

MEM is the same as MEMQ except that it takes a predicate of two
arguments, which is used for the comparison instead of EQ.

(MEM 'EQ A B) is the same as (MEMQ A B).
(MEM 'EQUAL A B) is the same as (MEMBER A B).

MEM is usually used with equality predicates other than EQ and EQUAL,
such as =, CHAR-EQUAL, or STRING-EQUAL.  It can also be used with
noncommutative predicates.  The predicate is called with ITEM as its
first argument and the element of LIST as its second argument, so:

  (MEM #'< 4 LIST)

finds the first element in LIST for which (< 4 x) is true; that is, it
finds the first element greater than 4.")

(define-documentation 'selectq 'function
  "A conditional that chooses one of its clauses to execute by
comparing the value of a form against various constants, which are
typically keyword symbols.  Its form is as follows:

  (SELECTQ key-form
    (test consequent consequent ...)
    (test consequent consequent ...)
    (test consequent consequent ...)
    ...)

The first thing SELECTQ does is to evaluate KEY-FORM; call the
resulting value KEY.  Then SELECTQ considers each of the clauses in
turn.  If KEY matches the clause's TEST, the consequents of this
clause are evaluated, and SELECTQ returns the value of the last
consequant.  If there are no matches, SELECTQ returns NIL.

A test can be any of the following:

A symbol         If the KEY is EQ to the symbol, it matches.
A number         If the KEY is EQ to the number, it matches.  Only small
                 numbers (integers) work.
A list           If the KEY is EQ to one of the elements of the list,
                 then it matches.  The elements of the list should be
                 symbols or integers.
T or OTHERWISE   The symbols T and OTHERWISE are special keywords that
                 match anything.  Either symbol can be used; T is
                 mainly for compatibility with Maclisp's CASEQ
                 construct.  To be useful, this should be the last
                 clause in the SELECTQ.

Note that the TEST arguments are not evaluated; if you want them to be
evaluated, use SELECT rather than SELECTQ.

Examples:

  (let ((voice 'tenor))
    (selectq voice
      (bass ``Barber of Seville'')
      (mezzo ``Carmen''))) => NIL
  
  (setq a 2) => 2
  (selectq a
    (1 ``one'')
    (2 ``two'')
    ((one two) ``1 2'')
    (otherwise ``not one or two'')) => ``two''")

(define-documentation 'neq 'function
  "(neq x y) = (not (eq x y)).

This is provided simply as an abbreviation for typing convenience.")

(define-documentation 'array-mem 'function
  "Similar to MEM except works on arrays.")

(define-documentation 'validate-function-spec 'function
  "Internal Zetalisp function that validates a function
specification.")

(define-documentation 'standardize-function-spec 'function
  "Internal Zetalisp function that standardizes a function
specification.")

(define-documentation 'find-body-declarations 'function
  "Internal Zetalisp function that parses the given body for DECLARE
forms.  It is used by the STRING functions in Symbolics.lisp.")

(define-documentation 'named-constant-p 'function
  "Zetalisp language tools function.  Returns T and the value of the
symbol given in NAME if the symbol is a constant.")

(define-documentation 'evaluate-constant 'function
  "Zetalisp language tools function.  Evaluates the given form if it
is a constant and has a value.")

(define-documentation 'coerce-string-arg 'function
  "Internal Zetalisp function.  Ensures the given argument in ARG-NAME
is a string.  If DESTRUCTIVE is T, then the type checking is done in a
destructive manner.")

(define-documentation 'defprop 'function
  "Give SYM's property list an INDICATOR-property corresponding to
VALUE.

After this is done, (GET SYM INDICATOR) returns VALUE.")

(define-documentation 'defselect 'function
  "Defines a function that is a select-method.  This function contains
a table of subfunctions; when it is called, the first argument, a
symbol on the keyword package called the message name, is looked up
in the table to determine which subfunction to call.  Each subfuction
can take a different number of argumebnts, and have a different
pattern of &optional and &rest arguments.  DEFSELECT is useful for a
variety of ``dispatching'' jobs.  By analogy with the more general
message passing facilities in flavors, the subfunctions are
sometimes called methods and the first argument is sometimes calle da
message.

The special form looks like:

  (defselect (function-spec default-handler no-which-operations)
    (message-name (args ...)
     body ...)
    (message-name (args ...)
     body ...)
    ...)

function-spec is the name of the function to be defined.
default-handler is optional; it must be a symbol and is a function
that gets called if the select-method is called with an unknown
message.  If default-handler is unsupplied or NIL, then an error
occurs if an unknown message is sent.  If no-which-operations is
non-NIL, the :which-operations method that would normally be supplied
automatically is suppressed.  The :which-operations method takes no
arguments and returns a list of all the message names in the
DEFSELECT.

The :operation-handled-p and :send-if-handles methods are
automatically supplied.

If function-spec is a symbol, and default-handler and
no-which-operations are not supplied, then the first subform of the
DEFSELECT can be just function-spec by itself, not enclosed in a list.

The remaining subforms in a DEFSELECT define methods.  message-name is
the message name, or a list of several message names if several
messages are to be handled by the same subfunction.  args is a
lambda-list; it should not include the first argument, which is the
message name.  body is the body of the function.

A method subform can instead look like:

  (message-name . symbol)

In this case, symbol is the name of a function that is called when the
message-name message is received.  It is called with the same
arguments as the select-method, including the message symbol itself.")

(define-documentation 'defsubst 'function
  "Defines inline functions.  It is used just like DEFUN and does
almost the same thing.

  (defsubst name lambda-list . body)

DEFSUBST defines a function that executes identically to the one that
a similar call to DEFUN would define.  The difference comes when a
function that calls this one is compiled.  Then, the call is
open-coded by substituting the inline function's definition into the
code being compiled.  Such a function is called an inline function.
For example, if we define:

  (defsubst square (x) (* x x))

  (defun foo (a b) (square (+ a b)))

then if FOO is interpreted, SQUARE works just as if it had been
defined by DEFUN.  If FOO is compiled, however, the squaring is
substituted into it and it compiles just like:

  (defun foo (a b) (* (+ a b) (+ a b)))

SQUARE could have been defined as:

  (defun square (x) (* x x))

  (proclaim '(inline square))

  (defun foo ...)")

(define-documentation 'string-append 'function
  "Copies and concatenates any number of strings into a single string.

STRINGS are strings or objects that can be coerced to strings.")

(define-documentation 'string-length 'function
  "Returns the number of characters in STRING.

STRING must be a string or an object that can be coerced into a
string.")

(define-documentation 'substring 'function
  "Extracts a substring of STRING, starting at the character specified
by FROM and going up to but not including the character specified by
TO.

The area in which the result would be consed on a Lisp Machine can be
optionally specified, but is ignored.  This is for compatibility with
Lisp Machine lisp.

The destructive version of SUBSTRING is NSUBSTRING.")

(define-documentation 'nsubstring 'function
  "This is the destructive version of SUBSTRING, except that it is not
destructive -- please treat it as such though, for compatibility with
Lisp Machine lisp.")

(define-documentation 'char-flipcase 'function
  "If CHAR is a lowercase alphabetic character in the standard
character set, CHAR-FLIPCASE returns its uppercase form.  If CHAR is
an uppercase alphabetic character in the standard character set,
CHAR-FLIPCASE returns its lowercase form.  Otherwise, it returns
CHAR.

  (char-flipcase #\X) => #\x
  (char-flipcase #\b) => #\B")

(define-documentation 'string-flipcase 'function
  "Returns a copy of STRING, with uppercase alphabetic characters
replaced by the corresponding lowercase characters, and with lowercase
alphabetic characters replaced by the corresponding uppercase
characters.

STRING is a string or an object that can be coerced to a string.

The keywords let you select portions of the string argumebtn for case
changing.  These keyword arguments must be non-negative integer
indices into the string array.  The result is always the same length
as STRING, however.


:start    Specifies the position within STRING from which to begin
          case changing (counting from 0).  Default is 0, the first
          character in the string.  :start must be <= :end.

:end      Specifies the position within STRING of the first character
          beyond the end of the case changing operation.  Default is
          NIL, that is, the operation continues to the end of the
          string.

Examples:

    (string-flipcase ``a sTrANGe UsE OF CaPitalS'')
    => ``A StRangE uSe of cApITALs''

    (string-flipcase 'symbol) => ``symbol''
    (string-flipcase 'symbol :start 2 :end 4) => ``SYmbOL''
    (string-flipcase ``End'' :start 2) => ``EnD''
    (string-flipcase ``STRing'') => ``strING''

The destructive version of STRING-FLIPCASE is the function
NSTRING-FLIPCASE.")

(define-documentation 'nstring-flipcase 'function
  "Reverses the alphabetic case in its argument: it changes uppercase
alphabetic characters to lowercase and lowerchase characters to
uppercase.  The original argumebnt, STRING, is modified and returned.

If STRING is not a string, an error is signalled.

The keywords let you select portions of the string argument for case
changing.  These keyword arguments must be non-negative integer
indices into the string array.  The result is always the same length
as STRING, however.

:start    Specifies the position within STRING from which to begin
          case changing (counting from 0).  Default is 0, the first
          character in the string.  :start must be <= :end.

:end      Specifies the position within STRING of the first character
          beyond the end of the case changing operation.  Default is
          NIL, that  is, the operation continues to the end of the
          string.

Examples:

    (nstring-flipcase ``a sTrANGe UsE OF CaPitalS'')
    => ``A StRangE uSe of cApITALs''

    (nstring-flipcase ``End'' :start 2) => ``EnD''
    (nstring-flipcase ``STRing'') => ``strING''
    (nstring-flipcase ``Guys and Dolls'' :start 4 :end nil)
    => ``Guys AND dOLLS''

The nondestructive version of NSTRING-FLIPCASE is the function
STRING-FLIPCASE>")

(define-documentation 'string-capitalize-words 'function
  "Returns a copy of STRING, such that hyphens are changed to spaces
and initial characters of each word are capitalized if they are
case-modifiable.

STRING is a string or an object that can be coerced to a string.

The keywords let you select portions of the string argument for
uppercasing.  These keyword arguments must be non-negative integer
indices into the string array.  The result is always the same length
as STRING, however.

:start    Specifies the position within STRING from which to begin
          uppercasing (counting from 0).  Default is 0, the first
          character in the string.  :start must be <= :end.

:end      Specifies the position within STRING of the first character
          beyond the end of the uppercasing operation.  Default is
          NIL, that is, the operation continues to the end of the
          string.

Examples:

    (string-capitalize-words ``string-capitalize-words'')
    => ``String Capitalize Words''

The destructive version of STRING-CAPITALIZE-WORDS is the function
NSTRING-CAPITALIZE-WORDS.")

(define-documentation 'nstring-capitalize-words 'function
  "Returns STRING, modified such that hyphens are changed to spaces
and initial characters of each word are capitalized if they are
case-modifiable.

If STRING is not a string, an error is signalled.

The keywords let you select portions of the string argument for
uppercasing.  These keyword arguments must be non-negative integer
indices into the string array.  The result is always the same length
as STRING, however.

:start    Specifies the position within STRING from which to begin
          uppercasing (counting from 0).  Default is 0, the first
          character in the string.  :start must be <= :end.

:end      Specifies the position within STRING of the first character
          beyond the end of the uppercasing operation.  Default is
          NIL, that is,  the operation continues to the end of the
          string.

Examples:

    (nstring-capitalize-words ``string-capitalize-words'')
    => ``String Capitalize Words''

The non-destructive version of NSTRING-CAPITALIZE-WORDS is the
function STRING-CAPITALIZE-WORDS.")

(define-documentation 'string-compare 'function
  "Compares two strings, or substrings of them.  The comparison is
case-insensitive, ignoring alphabetic cases.

STRING-COMPARE returns:

  * a positive number if STRING1 > STRING2
  * zero if STRING1 = STRING2
  * a negative number if STRING1 < STRING2

If the strings are not equal, the absolute value of the number
returned is one more than the index (in STRING1) at which the
difference occured.

The keywords let you specify substrings of the two string arguments
for comparison.  These keyword arguments must be non-negative integer
indices into the string array.

:start1   Specifies the position within STRING1 from which to begin
          the comparison (counting from 0).  Default is 0, the first
          character in the string.  :start1 must be <= :end1.  If the
          value of :start1 is non-zero, the magnitude of the answer is
          relative to the beginning of STRING1, not to the beginning
          of the substring being compared.

:end1     Specifies the position within STRING1 of the first character
          beyond the end of the comparison.  Default is NIL, that is,
          the operation continues to the end of the string.

:start2   Work in analogous fashion for STRING2.
:end2     Work in analogous fashion for STRING2.

The case-sensitive version of STRING-COMPARE is the function
STRING-EXACT-COMPARE.")

(define-documentation 'string-exact-compare 'function
  "Compares two strings, or substrings of them, exactly including the
  character fields and alphabetic case.

STRING-EXACT-COMPARE returns:

  * a positive number if STRING1 > STRING2
  * zero if STRING1 = STRING2
  * a negative number if STRING1 < STRING2

If the strings are not equal, the absolute value of the number
returned is one more than the index (in STRING1) at which the
difference occured.

The keywords let you specify substrings of the two string arguments
for comparison.  These keyword arguments must be non-negative integer
indices into the string array.

:start1   Specifies the position within STRING1 from which to begin
          the comparison (counting from 0).  Default is 0, the first
          character in the string.  :start1 must be <= :end1.  If the
          value of :start1 is non-zero, the magnitude of the answer is
          relative to the beginning of STRING1, not to the beginning
          of the substring being compared.

:end1     Specifies the position within STRING1 of the first character
          beyond the end of the comparison.  Default is NIL, that is,
          the operation continues to the end of the string.

:start2   Work in analogous fashion for STRING2.
:end2     Work in analogous fashion for STRING2.

Examples:

    (string-exact-compare ``aaa'' ``aaa'') => 0
    (string-exact-compare ``yo'' ``YO'') => 1
    (string-exact-compare ``this is it'' ``This Is it'') => 1
    (string-exact-compare #\d ``mmm..'') => -1

The case-insensitive version of STRING-EXACT-COMPARE is the predicate
STRING-COMPARE.")

(define-documentation 'string-reverse 'function
  "Creates and returns a copy of STRING with the order of characters
reversed.  This reverses a one-dimensional array of any type.  If
STRING is not a string or another one-dimensional array, it is coerced
into a string.

The keywords let you select portions of the string argument for
reversing.  These keyword arguments must be non-negative integer
indices into the string array.  The entire argument, STRING, is
returned, however.

:start    Specifies the position within STRING from which to begin
          reversing (counting from 0).  Default is 0, the first
          character in the string.  :start must be <= :end.

:end      Specifies the position within STRING of the first character
          beyond the end of the reversing operation.  Default is NIL,
          that is, the operation continues to the end of the string.

Examples:

    (string-reverse #\a) => ``a''
    (string-reverse 'symbol) => ``LOBMYS''
    (string-reverse ``a string'') => ``gnirts a''
    (string-reverse ``end'' :start 1) => ``edn''
    (string-reverse ``start'' :end 3) => ``atsrt''
    (string-reverse ``middle'' :start 1 :end 5) => ``mlddie''

The destructive version of STRING-REVERSE is the function
STRING-NREVERSE.")

(define-documentation 'string-nreverse 'function
  "Returns STRING with the order of characters reversed, modifying the
original string, rather than creating a new one.  This reverses a
one-dimentional array of any type.  If STRING is a character, it is
simply returned.

STRING is a string, a one-dimensional array, or an object that can be
coerced to a string. Since STRING-NREVERSE is destructive, coercion
should be used with care since a string internal to the object might
be modified.

The keywords let you select portions of the string argument for
reversing.  These keyword arguments must be non-negative integer
indices into the string array.  The entire argument, STRING, is
returned, however.

:start    Specifies the position within STRING from which to begin
          reversing (counting from 0).  Default is 0, the first
          character in the string.  :start must be <= :end.

:end      Specifies the position within STRING of the first character
          beyond the end of the reversing operation.  Default is NIL,
          that is, the operation continues to the end of the string.

The non-destructive version of STRING-NREVERSE is the function
STRING-REVERSE.")

(define-documentation 'string-search-char 'function
  "Searches STRING looking for the character CHAR.  The search uses
CHAR-EQUAL, which ignores the character fields and alphabetic case.

STRING-SEARCH-CHAR returns NIL if it does not find CHAR; if
successful, it returns the position of the first occurrence of CHAR.
To reverse the search, returning the position of the last occurrence
of CHAR in the (sub)string searched, set :from-end to T.

CHAR must be a character object.

STRING must be a string, or an object that can be coerced to a string.

The keywords let you specify the parts of STRING to be searched.
These keyword arguments must be non-negative integer indices into the
string array.

:from-end If set to a non-NIL value, returns the position of the last
          occurrence of char in the string or the specified substring.

:start    Specifies the position within STRING from which to begin the
          search (counting from 0).  Default is 0, the first character
          in the string.  :start must be <= :end.

:end      Specifies the position within STRING of the first character
          beyond the end of the search.  Default is NIL, that is the
          entire length of STRING is searched.

Examples:

    (string-search #\? ``banana'') => NIL
    (string-search-char #\a ``banana'') => 1
    (string-search-char #\a ``banana'' :from-end t) => 5

The case-sensitive version of STRING-SEARCH-CHAR is the function
STRING-SEARCH-EXACT-CHAR.")

(define-documentation 'string-search-exact-char 'function
  "Searches STRING looking for the character, CHAR.  The search
compares all characters exactly, using all character fields including
alphabetic case.

STRING-SEARCH-EXACT-CHAR returns NIL if it does not find CHAR; if
successful, it returns the position of the first occurrence of CHAR in
the string or substring reached.  To reverse the search returning the
position of the last occurrence of CHAR in the (sub)string searched,
specify a non-NIL value for the keyword :from-end.

CHAR must be a character object.

STRING must be a string, or an object that can be coerced to a string.

The keywords let you specify the parts of STRING to be searched.
These keyword arguments must be non-negative integer indices into the
string array.

:from-end If set to a non-NIL value, returns the position of the last
          occurrence of char in the string or the specified substring.

:start    Specifies the position within STRING from which to begin the
          search (counting from 0).  Default is 0, the first character
          in the string.  :start must be <= :end.

:end      Specifies the position within STRING of the first character
          beyond the end of the search.  Default is NIL, that is the
          entire length of STRING is searched.

The case-insensitive version of STRING-SEARCH-EXACT-CHAR is the
function STRING-SEARCH-CHAR.")

(define-documentation 'string-search-not-char 'function
  "Searches STRING looking for occurrences of any character other than
CHAR.  The search uses CHAR-EQUAL, which ignores alphabetic case.

STRING-SEARCH-NOT-CHAR returns NIL, or the position of the first
occurrence of any character that is not CHAR.  To reverse the search,
returning the position of the last occurrence of a character other
than CHAR in the (sub)string searched, specify T for the keyword
argument :from-end.

CHAR must be a character object.

STRING must be a string, or an oibject that can be coerced to a
string.

The keywords let you specify the parts of STRING to be searched.
These keyword arguments must be non-negative integer indices into the
string array.

:from-end If set to a non-NIL value, returns the position of the last
          occurrence of char in the string or the specified substring.

:start    Specifies the position within STRING from which to begin the
          search (counting from 0).  Default is 0, the first character
          in the string.  :start must be <= :end.

:end      Specifies the position within STRING of the first character
          beyond the end of the search.  Default is NIL, that is the
          entire length of STRING is searched.

Examples:

    (string-search-not-char #\E ``eel'') => 2
    (string-search-not-char #\l ``oscillate'') => 0

    (string-search-not-char #\l ``oscillate'' :start 5 :from-end t)
    => 8

    (string-search-not-char #\l ``oscillate''
                            :start 2
                            :end 5
                            :from-end t)
    => 3

The case-sensitive version of STRING-SEARCH-NOT-CHAR is the function
STRING-SEARCH-NOT-EXACT-CHAR.")

(define-documentation 'string-search-not-exact-char 'function
  "Searches STRING looking for occurrences of any character other than
CHAR.  The search compares all characters exactly.

STRING-SEARCH-NOT-EXACT-CHAR returns NIL, or the position of the first
occurrence of any character that is not CHAR.  To reverse the search,
returning the position of the last occurrence of a character other
than CHAR in the (sub)string searched, specify T for the keyword
:from-end.

CHAR must be a character object.

STRING must be a string, or an oibject that can be coerced to a
string.

The keywords let you specify the parts of STRING to be searched.
These keyword arguments must be non-negative integer indices into the
string array.

:from-end If set to a non-NIL value, returns the position of the last
          occurrence of char in the string or the specified substring.

:start    Specifies the position within STRING from which to begin the
          search (counting from 0).  Default is 0, the first character
          in the string.  :start must be <= :end.

:end      Specifies the position within STRING of the first character
          beyond the end of the search.  Default is NIL, that is the
          entire length of STRING is searched.

Examples:

    (string-search-not-exact-char #\a ``AAA'') => 0
    (string-search-not-exact-char #\a ``bbba'') => 0
    (string-search-not-exact-char #\a ``aaabAcBA'') => 3

    (string-search-not-exact-char #\a ``abbacccaccca''
                                  :from-end 3
                                  :start 2
                                  :end 9)
    => 8

The case-insensitive version of STRING-SEARCH-NOT-EXACT-CHAR is the
function STRING-SEARCH-NOT-CHAR.")

(define-documentation 'string-search-set 'function
  "Searches STRING looking for a character that is in CHAR-SET.  The
search uses CHAR-EQUAL, which ignores alphabetic case.

STRING-SEARCH-SET returns NIL, or the position of the first character
that is CHAR-EQUAL to some element of the CHAR-SET.  To reverse the
search, returning the position of the last occurrence of the initial
character of CHAR-SET in the (sub)string searched, set :from-end to T.

CHAR-SET is a set of characters which can be represented as a list of
characters, an array of characters, or a string of characters.

STRING must be a string, or an oibject that can be coerced to a
string.

The keywords let you specify the parts of STRING to be searched.
These keyword arguments must be non-negative integer indices into the
string array.

:from-end If set to a non-NIL value, returns the position of the last
          occurrence of char in the string or the specified substring.

:start    Specifies the position within STRING from which to begin the
          search (counting from 0).  Default is 0, the first character
          in the string.  :start must be <= :end.

:end      Specifies the position within STRING of the first character
          beyond the end of the search.  Default is NIL, that is the
          entire length of STRING is searched.

Examples:

    (string-search-set #(#\a) ``aaa'') => 0
    (string-search-set '(#\h #\i) ``hi'') => 0
    (string-search-set '(#\a) ``abcdefabc'') => 0
    (string-search-set #(#\a #\. #\h) ``ping...ahh...haaa'') => 4")

(define-documentation 'string-search-not-set 'function
  "Searches STRING looking for a character that is not in CHAR-SET.
The search uses CHAR-EQUAL, which ignores alphabetic case.

STRING-SEARCH-NOT-SET returns NIL, or the position of the first
character that is not CHAR-EQUAL to some element of the CHAR-SET.  To
reverse the search, returning the position of the last occurrence of a
character not in CHAR-SET in the (sub)string searched, specify T for
the keyword argument :from-end.

CHAR-SET is a set of characters which can be represented as a list of
characters, an array of characters, or a string of characters.

STRING must be a string, or an oibject that can be coerced to a
string.

The keywords let you specify the parts of STRING to be searched.
These keyword arguments must be non-negative integer indices into the
string array.

:from-end If set to a non-NIL value, returns the position of the last
          occurrence of char in the string or the specified substring.

:start    Specifies the position within STRING from which to begin the
          search (counting from 0).  Default is 0, the first character
          in the string.  :start must be <= :end.

:end      Specifies the position within STRING of the first character
          beyond the end of the search.  Default is NIL, that is the
          entire length of STRING is searched.")

(define-documentation 'string-search 'function
  "Searches STRING looking for occurrences of KEY.  The search uses
CHAR-EQUAL which ignores alphabetic case.

STRING-SEARCH returns NIL, or the position of the first character of
KEY occurring in the (sub)string.  To reverse the search, returning
the position of the last occurrence of the initial KEY character in
the (sub)string searched, specify a non-NIL value for :from-end.

KEY and STRING must be strings, or objects that can be coerced to a
string.

The keywords let you specify parts of STRING to be searched, as well
as the parts of KEY to search for.  These keyword arguments must be
non-negative integer indices into the string array.

:from-end If a non-NIL value is specified, returns the position of the
          first character of the last occurrence of KEY in the string
          or the specified substring.

:start1   Specifies the position within KEY from which to begin the
          search (counting from 0).  Default is 0, the first character
          in the string.  :start1 must be <= :end1.

:end1     Specifies the position within KEY of the first character
          beyond the end of the search.  Default is NIL, that is the
          entire length of KEY is used.

:start2   Work analogously for STRING.
:end2     Work analogously for STRING.

Examples:

    (string-search ``es'' ``witches'') => 5
    (string-search ``es'' ``tresses'') => 2
    (string-search ``es'' ``tresses'' :from-end t) => 5
    (string-search ``er'' ``tresses'') => NIL
    (string-search ``es'' ``tresses'' :start2 3) => 5

The case-sensitive version of STRING-SEARCH is the function
STRING-SEARCH-EXACT.")

(define-documentation 'string-search-exact 'function
  "Searches STRING looking for occurrences of KEY.  The search
compares alphabetic case.

STRING-SEARCH-EXACT returns NIL, or the position of the first
character of KEY occurring in the (sub)string.  To reverse the search,
returning the position of the last occurrence of the initial KEY
character in the (sub)string searched, specify a non-NIL value for
:from-end.

KEY and STRING must be strings, or objects that can be coerced to a
string.

The keywords let you specify parts of STRING to be searched, as well
as the parts of KEY to search for.  These keyword arguments must be
non-negative integer indices into the string array.

:from-end If a non-NIL value is specified, returns the position of the
          first character of the last occurrence of KEY in the string
          or the specified substring.

:start1   Specifies the position within KEY from which to begin the
          search (counting from 0).  Default is 0, the first character
          in the string.  :start1 must be <= :end1.

:end1     Specifies the position within KEY of the first character
          beyond the end of the search.  Default is NIL, that is the
          entire length of KEY is used.

:start2   Work analogously for STRING.
:end2     Work analogously for STRING.

Examples:

    (string-search-exact #\a ``AAA'') => NIL
    (string-search-exact #\a ``bbbabba'') => 3
    (string-search-exact #\a ``abbbacccbaddda'' :from-end 2) => 13

The case-insensitive version of STRING-SEARCH-EXACT is the function
STRING-SEARCH.")

(define-documentation 'string-pluralize 'function
  "Returns a copy of its string argument containing the plural of the
word in STRING.  Any added characters go in the same case as the last
character of STRING.

STRING is a string or an object that can be coerced to a string.

Examples:

   (string-pluralize ``event'') => ``events''
   (string-pluralize ``Man'') => ``Men''
   (string-pluralize ``Can'') => ``Cans''
   (string-pluralize ``key'') => ``keys''
   (string-pluralize ``TRY'') => ``TRIES''

For words with multiple plural forms depending on the meaning,
STRING-PLURALIZE cannot always do the right thing.")

(define-documentation 'string-pluralize-to-stream 'function
  "Pluralizes the string in STRING with STRING-PLURALIZE, with the
result being placed on the stream given in STREAM.")

(define-documentation 'string-a-or-an 'function
  "Computes whether the article ``a'' or ``an'' is used when
introducing a noun.  If BOTH-WORDS is true, the result is the
concatenation  of the article, a space, and the NOUN; otherwise, the
article is returned.

Examples:

    (string-a-or-an 'rock) => ``a ROCK''
    (string-a-or-an 'rock t :upcase) => ``A ROCK''
    (string-a-or-an ``egg'') => ``an egg''")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ symbols.lisp:

(define-documentation 'ensure-symbol 'function
    "Returns a symbol with name designated by NAME, accessible in
package designated by PACKAGE. If symbol is not already accessible in
PACKAGE, it is interned there. Returns a secondary value reflecting
the status of the symbol in the package, which matches the secondary
return value of INTERN.

Example: (ENSURE-SYMBOL :CONS :CL) => CL:CONS, :EXTERNAL")

(define-documentation 'maybe-intern 'function
  "Interns NAME if PACKAGE exists, otherwise makes a symbol.")

(define-documentation 'format-symbol 'function
  "Constructs a string by applying ARGUMENTS to CONTROL as if by
FORMAT, and then creates a symbol named by that string. If PACKAGE is
NIL, returns an uninterned symbol, if package is T, returns a symbol
interned in the current package, and otherwise returns a symbol
interned in the package designated by PACKAGE.")

(define-documentation 'make-keyword 'function
  "Interns the string designated by NAME in the KEYWORD package.")

(define-documentation 'make-gensym 'function
  "If NAME is a non-negative integer, calls GENSYM using it. Otherwise
NAME must be a string designator, in which case calls GENSYM using the
designated string as the argument.")

(define-documentation 'make-gensym-list 'function
  "Returns a list of LENGTH gensyms, each generated as if with a call
to MAKE-GENSYM, using the second (optional, defaulting to \"G\")
argument.")

(define-documentation 'symbolicate 'function
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package.")

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ types.lisp:



;;;}}}
;;; ------------------------------------------------------------------

;;;}}}
;;; ==================================================================

;;; documentation.lisp ends here
