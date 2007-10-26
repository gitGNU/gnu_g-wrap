;;;; File: c-types.scm
;;;; Copyright (C) 2004, 2007 Andreas Rottmann
;;;; Copyright (C) 2005, 2006 Ludovic Court�s
;;;;
;;;; based upon G-Wrap 1.3.4,
;;;;   Copyright (C) 1996, 1997,1998 Christopher Lee
;;;;   Copyright (C) 1999, 2000, 2001, 2002 Rob Browning
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this software; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, 675 Mass Ave, Cambridge,
;;;; MA 02139, USA.
;;;;

;;; Commentary:
;;
; This modules defines an interface to the basic C types (char, int,
; long, double, ... and C strings). Also supported are opaque, typed
; pointers called WCP (wrapped C pointer).
;;
;;; Code:

(define-module (g-wrap c-types)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-39)

  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap util)
  #:use-module (g-wrap rti)

  #:export (wrap-simple-type!

	    <gw-ranged-integer-type>
	    wrap-ranged-integer-type!

	    <gw-ctype-void> <gw-ctype-mchars>

	    <gw-wct>
            gw-wcts-nullable?
	    wrap-as-wct!
	    wcp-mark-function wcp-free-function wcp-equal-predicate))

(define-generic wrap-simple-type!)


;;;
;;; Ranged integers
;;;

(define-class <gw-ranged-integer-type> (<gw-simple-rti-type>)
  (min #:init-keyword #:min #:init-value #f)
  (max #:init-keyword #:max)
  min-var
  max-var)

(define-generic wrap-ranged-integer-type!)

(define-method (initialize (type <gw-ranged-integer-type>) initargs)
  (next-method)
  (let ((c-sym-name (any-str->c-sym-str (c-type-name type))))
    (slot-set! type 'min-var
	       (gen-c-tmp (string-append "range_minval" c-sym-name)))
    (slot-set! type 'max-var
	       (gen-c-tmp (string-append "range_minval" c-sym-name)))))

(define-method (default-c-value-for-type (type <gw-ranged-integer-type>))
  ;; Default value to initialize C variables of such types.
  "0")

 ;; void is class of its, own, of course ;-)
(define-class <gw-ctype-void> (<gw-simple-rti-type>))

(define-method (initialize (void <gw-ctype-void>) initargs)
  (next-method void (append '(#:needs-result-var? #f) initargs)))

(define-method (unwrap-value-cg (type <gw-ctype-void>)
				(value <gw-value>) error-var
				(inlined? <boolean>))
  '())

(define-method (pre-call-arg-cg (type <gw-ctype-void>)
				(param <gw-value>)
				status-var)
  (error "Can't use void as an argument type."))

(define-method (post-call-arg-cg (type <gw-ctype-void>)
				 (param <gw-value>)
				 status-var)
  (error "Can't use void as an argument type."))

;; no result assignment.
(define-method (call-cg (type <gw-ctype-void>)
			(result <gw-value>)
			func-call-code
			status-var)
  (list func-call-code ";\n"))

(define-class <gw-ctype-mchars> (<gw-rti-type>)
  #:allowed-options '(null-ok))

(define-method (destroy-value-cg (type <gw-ctype-mchars>)
				 (value <gw-value>)
				 error-var
				 (inlined? <boolean>))
  (next-method)) ;; no need, we already strdup'd it if necessary

(define-method (global-declarations-cg (wrapset <gw-wrapset>)
				       (mchars <gw-ctype-mchars>))
  (list
   (next-method)
   "#include <string.h>\n"))


(define-method (client-global-declarations-cg (wrapset <gw-wrapset>)
					      (mchars <gw-ctype-mchars>))
  (list "#include <string.h>\n"))


(define-method (global-declarations-cg (wrapset <gw-wrapset>)
				       (integer <gw-ranged-integer-type>))
  (list
   (next-method)
   "#include <limits.h>\n"
   "#if (__STDC_VERSION__ >= 199901L)\n"
   "/* <stdint.h> is a C99 header.  It defines SIZE_MAX among\n"
   "   other things.  */\n"
   "# include <stdint.h>\n"
   "#endif\n"))


(define-method (set-value-cg (type <gw-ctype-mchars>)
			     (lvalue <gw-value>) (rvalue <string>))
  (if (string=? rvalue "NULL")
      (list (var lvalue) " = NULL;\n")
      (list (var lvalue) " = strdup(" rvalue ");\n")))


;;;
;;; Wrapped C Types
;;;

(define-class <gw-wct> (<gw-rti-type>)
  ;; Pointer to the C function that should be called during the GC mark phase
  ;; (if any) of a wrapped C pointer.
  (wcp-mark-function #:accessor wcp-mark-function
		     #:init-keyword #:wcp-mark-function
		     #:init-value "NULL")

  ;; Pointer to the C function that should be called when a wrapped C pointer
  ;; (WCP) is about to be garbage collected.
  (wcp-free-function #:accessor wcp-free-function
		     #:init-keyword #:wcp-free-function
		     #:init-value "NULL")

  ;; Pointer to a C function that returns an non-zero integer when passed two
  ;; C pointers that are `equal'  in the sense of that type.
  (wcp-equal-predicate #:accessor wcp-equal-predicate
		       #:init-keyword #:wcp-equal-predicate
		       #:init-value "NULL")

  #:allowed-options '(null-ok))

(define-method (initialize (wct <gw-wct>) initargs)
  (next-method wct (cons #:ffspec (cons 'pointer initargs))))

;; Are WCTs nullable by default?
(define gw-wcts-nullable? (make-parameter #f))

(define-method (make-typespec (type <gw-wct>) (options <list>))
  (next-method type (if (gw-wcts-nullable?)
                        (if (memq 'non-null options)
                            (cons 'caller-owned (delq 'non-null options))
                            (append '(null-ok caller-owned) options))
                        (cons 'caller-owned options))))

(define-method (check-typespec-options (type <gw-wct>) (options <list>))
  (let ((remainder (lset-difference eq? options (append '(caller-owned const)
                                                        (slot-ref type 'allowed-options)))))
    (if (not (null? remainder))
	(raise-bad-typespec type options "spurious options: ~S" remainder))))

(define-method (wrap-as-wct! (wrapset <gw-wrapset>) . args)
  (let ((wct (apply make <gw-wct> args)))
    (add-type! wrapset wct)
    wct))
