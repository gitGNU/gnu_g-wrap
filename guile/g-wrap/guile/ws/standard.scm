;;;; File: standard.scm
;;;; Copyright (C) 2004-2006 Andreas Rottmann
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
; Guile-specific part of the standard wrapset.
;;
;;; Code:

(define-module (g-wrap guile ws standard)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap util)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap ws standard)
  #:use-module (g-wrap guile))


;;; standard wrapset

(define-class <gw-guile-ctype-void> (<gw-ctype-void> <gw-guile-rti-type>))
(define-class <gw-guile-ctype-mchars> (<gw-ctype-mchars> <gw-guile-rti-type>))

(define-class <standard-wrapset> (<gw-guile-wrapset>
				  <gw-standard-wrapset>)
   #:id 'standard
   #:types `((void ,<gw-guile-ctype-void>)
	     (mchars ,<gw-guile-ctype-mchars>)))

(define-method (initialize (wrapset <standard-wrapset>) initargs)
  (next-method)

  (set! (module wrapset) '(g-wrap gw standard))

  ;; SCM - pass scheme pointers through unmolested.
  (wrap-simple-type! wrapset
		     #:name 'scm
		     #:c-type-name "SCM"
		     #:type-check '("1")
		     #:ffspec 'pointer ;; FIXME: not accurate
		     #:unwrap '(c-var " = " scm-var ";\n")
		     #:wrap '(scm-var " = " c-var ";\n"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:wct> - wrapped c pointer type object
  (wrap-simple-type! wrapset
		     #:name '<gw:wct>
		     #:c-type-name "SCM"
		     #:type-check '("gw_wct_p(" scm-var ")")
		     #:unwrap '(c-var " = " scm-var ";\n")
		     #:wrap '(scm-var " = " c-var ";\n")
		     #:ffspec 'pointer) ;; not accurate

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:wcp> - wrapped c pointer object
  (wrap-simple-type! wrapset
		     #:name '<gw:wcp>
		     #:c-type-name "SCM"
		     #:type-check '("gw_wcp_p(" scm-var ")")
		     #:unwrap '(c-var " = " scm-var ";\n")
		     #:wrap '(scm-var " = " c-var ";\n")
		     #:ffspec 'pointer) ;; not accurate

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:void*> - wrapped c pointer object
  (wrap-as-wct! wrapset
		#:name '<gw:void*>
		#:c-type-name "void *"
		#:c-const-type-name "const void *")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Wrapped functions...

  (wrap-function! wrapset
		  #:name  'gw:wct?
		  #:returns 'bool
		  #:c-name "gw_wct_p"
		  #:arguments '((scm obj))
		  #:description "Is obj a gw:wct?")

  (wrap-function! wrapset
		  #:name 'gw:wcp?
		  #:returns 'bool
		  #:c-name "gw_wcp_p"
		  #:arguments '((scm obj))
		  #:description "Is obj a gw:wcp?")

  (wrap-function! wrapset
		  #:name 'gw:wcp-is-of-type?
		  #:returns 'bool
		  #:c-name "gw_wcp_is_of_type_p"
		  #:arguments '((<gw:wct> type) (<gw:wcp> wcp))
		  #:description
"Returns #f iff the given wcp is not of the type specified.  type must be a
g-wrap wrapped c type object, usually available via global bindings.  For
example (gw:wcp-is-a? <gw:void*> foo)")

  (wrap-function! wrapset
		  #:name 'gw:wcp-coerce
		  #:returns '<gw:wcp>
		  #:c-name "gw_wcp_coerce"
		  #:arguments '((<gw:wcp> wcp) (<gw:wct> new-type))
		  #:description "Coerce the given wcp to new-type.  This can be dangerous, so be careful.")

  (wrap-function! wrapset
		  #:name '%gw:procedure->method-public
		  #:returns 'void
		  #:arguments '((scm proc) (scm class_name)
				(scm generic-name) (scm n_req_args)
				(scm use_optional_args))
		  #:c-name "gw_guile_procedure_to_method_public"))

(define-method (add-type! (wrapset <standard-wrapset>)
			  (type <gw-guile-simple-rti-type>))
  (let ((info (assq-ref
	       '((scm) (<gw:wct>) (<gw:wcp>)
		 (bool #f
		       (c-var "= SCM_NFALSEP(" scm-var ");\n")
		       (scm-var "= (" c-var ") ? SCM_BOOL_T : SCM_BOOL_F;\n")
		       <boolean>)

		 (char ("SCM_NFALSEP(scm_char_p(" scm-var "))")
		       (c-var "= SCM_CHAR(" scm-var ");\n")
		       (scm-var "= SCM_MAKE_CHAR(" c-var ");\n")
		       <char>)

		 (unsigned-char ("SCM_NFALSEP(scm_char_p(" scm-var "))")
				(c-var "= SCM_CHAR(" scm-var ");\n")
				(scm-var "= SCM_MAKE_CHAR(" c-var ");\n")
				<char>)

		 (float ("SCM_NFALSEP(scm_number_p(" scm-var "))")
			(c-var "= (float)scm_to_double (" scm-var ");\n")
			(scm-var "= scm_from_double((double)" c-var ");\n")
			<real>)

		 (double ("SCM_NFALSEP(scm_number_p(" scm-var "))\n")
			 (c-var "= scm_to_double (" scm-var ");\n")
			 (scm-var "= scm_from_double (" c-var ");\n")
			 <real>))
	       (name type))))
    (cond ((null? info)
	   (next-method))
	  ((not info)
	   ;; FIXME: Use a condition
	   (error "attempt to wrap unknown simple type" (name type)))
	  (else
	   (slot-set! type 'type-check (list-ref info 0))
	   (slot-set! type 'unwrap (list-ref info 1))
	   (slot-set! type 'wrap (list-ref info 2))
	   (slot-set! type 'class-name (list-ref info 3))
	   (next-method)))))


;;;
;;; <gw-guile-ctype-void>
;;;

(define-method (wrap-value-cg (type <gw-guile-ctype-void>)
			      (value <gw-value>) error-var
			      (inlined? <boolean>))
  (list (scm-var value) " = SCM_UNSPECIFIED;\n"))

(define-method (post-call-result-cg (type <gw-guile-ctype-void>)
				    (result <gw-value>)
				    status-var)
  (list (scm-var result) " = SCM_UNSPECIFIED;\n"))

;;;
;;; <gw-guile-ranged-integer-type>
;;;
(define-class <gw-guile-ranged-integer-type> (<gw-ranged-integer-type>
					      <gw-guile-rti-type>)
  (wrap #:init-keyword #:wrap)
  (unwrap #:init-keyword #:unwrap))

(define <ranged-integer-type> <gw-guile-ranged-integer-type>) ; Lazy ;)

;; Returns a string, representing the guile "name" of the type. The
;; "name" will be used to build the names of the conversion functions
;; for that type. This means the types don't have to exactly match,
;; but the "name" returned must indictate a type large enough to hold
;; the type @var{type}.
(define (ranged-integer-name type)
  (let ((special (assq-ref '((unsigned-short . "ushort")
			     (unsigned-int . "uint")
			     (unsigned-long . "ulong")
			     (long-long . "long_long")
			     (unsigned-long-long . "ulong_long")
			     (int8 . "short")
			     (int16 . "int")
			     (int32 . "long")
			     (int64 . "long_long")
			     (unsigned-int8 . "ushort")
			     (unsigned-int16 . "uint")
			     (size_t . "ulong")
			     (ssize_t . "long")
			     (unsigned-int32 . "ulong")
			     (unsigned-int64 . "ulong_long"))
			   (name type))))
    (if special
	special
	(symbol->string (name type)))))

(define-method (wrap-ranged-integer-type! (wrapset <standard-wrapset>) . args)
  (let* ((type (apply make <ranged-integer-type> args))
	 (name (ranged-integer-name type)))
    (slot-set! type 'wrap (string-append "scm_from_" name))
    (slot-set! type 'unwrap (string-append "scm_to_" name))
    (slot-set! type 'class-name '<integer>)
    (add-type! wrapset type)))

(define-method (add-type! (wrapset <standard-wrapset>)
			  (type <ranged-integer-type>))
  (next-method)
  (slot-set! wrapset 'use-limits? #t))

(define-method (initialize (type <ranged-integer-type>) initargs)
  (next-method)
  (let ((c-sym-name (any-str->c-sym-str (c-type-name type))))
    (slot-set! type 'min-var
	       (gen-c-tmp (string-append "range_minval" c-sym-name)))
    (slot-set! type 'max-var
	       (gen-c-tmp (string-append "range_minval" c-sym-name)))))

(define-method (wrap-value-cg (type <ranged-integer-type>)
			      (value <gw-value>)
			      error-var
			      (inlined? <boolean>))
  (list (scm-var value) " = " (slot-ref type 'wrap) "(" (var value) ");\n"))

(define-method (unwrap-value-cg (type <ranged-integer-type>)
				(value <gw-value>)
				error-var
				(inlined? <boolean>))
  (let ((scm-var (scm-var value))
	(c-var (var value))
	(minvar (slot-ref type 'min-var))
	(maxvar (slot-ref type 'max-var)))
    (list "if(SCM_FALSEP(scm_integer_p(" scm-var ")))"
	  `(gw:error ,error-var type ,(wrapped-var value))
	  (if (slot-ref type 'min)
	      (list
	       "else if(SCM_FALSEP(scm_geq_p(" scm-var ", " minvar "))"
	       "        || SCM_FALSEP(scm_leq_p(" scm-var ", " maxvar ")))")
	      (list
	       "else if(SCM_NFALSEP(scm_negative_p(" scm-var "))"
	       "        || SCM_FALSEP(scm_leq_p(" scm-var ", " maxvar ")))"))
	  `(gw:error ,error-var range ,(wrapped-var value))
	  "else {\n"
	  ;; here we pass NULL and 0 as the callers because we've already
	  ;; checked the bounds on the argument
	  "  " c-var " = " (slot-ref type 'unwrap) "(" scm-var ");\n"
	  "}\n")))


(define-method (global-declarations-cg
				       (wrapset <gw-guile-wrapset>)
				       (type <ranged-integer-type>))
;  (format #t "global-declarations-cg/guile-wrapset~%")
  (list
   (next-method)
   (if (slot-ref type 'min)
       (list "static SCM " (slot-ref type 'min-var) ";\n")
       '())
   "static SCM " (slot-ref type 'max-var) ";\n"))

(define (minmax-var-init-cg type)
  (let ((minvar (slot-ref type 'min-var))
	(maxvar (slot-ref type 'max-var))
	(minval (slot-ref type 'min))
	(maxval (slot-ref type 'max)))
    (list
     (if minval
	 (list minvar " = " (slot-ref type 'wrap) "(" minval ");\n"
	       "scm_gc_protect_object(" minvar ");\n")
	 '())
     maxvar " = " (slot-ref type 'wrap) "(" maxval ");\n"
     "scm_gc_protect_object(" maxvar ");\n")))

(define-method (initializations-cg (wrapset <gw-guile-wrapset>)
				   (type <ranged-integer-type>)
				   error-var)
  (list
   (next-method)
   (minmax-var-init-cg type)))

(define-method (client-global-declarations-cg (wrapset <gw-guile-wrapset>)
					      (type <ranged-integer-type>))
  (global-declarations-cg wrapset type))

(define-method (client-initializations-cg (wrapset <gw-guile-wrapset>)
					  (type <ranged-integer-type>)
					  error-var)
  (minmax-var-init-cg type))


;;;
;;; <gw-guile-ctype-mchars>
;;;

(define-method (initialize (self <gw-guile-ctype-mchars>) initargs)
  (next-method self (append '(#:class-name <string>) initargs)))

(define-method (parse-typespec-option! (typespec <gw-typespec>)
				       (type <gw-guile-ctype-mchars>)
				       (option <symbol>))
  (next-method)
  (if (eq? 'null-ok option)
      (add-option! typespec 'unspecialized))) ;; can't pass #f for <string>

(define-method (wrap-value-cg (type <gw-guile-ctype-mchars>)
			      (value <gw-value>)
			      error-var
			      (inlined? <boolean>))
  (list
   "if (" (var value) " == NULL) " (scm-var value) " = SCM_BOOL_F;\n"
   "else "
   (scm-var  value)
   (let* ((ts (typespec value))
	  (opts (if ts (options ts) '())))
     ;; When the string is `out' and `caller-owned', we must take the C
     ;; string, i.e., take control over its underlying memory.
     (if (and (memq 'caller-owned opts) (memq 'out opts))
	 " = scm_take_locale_string ("
	 " = scm_from_locale_string ("))
   (var value) ");\n"))

(define-method (unwrap-value-cg (type <gw-guile-ctype-mchars>)
				(value <gw-value>)
				error-var
				(inlined? <boolean>))
  (let ((c-var (var value))
	(scm-var (scm-var value))
	(c-size-var (gen-c-tmp "_size")))
    (let ((unwrap-code
	   (list
	    "if (scm_is_string (" scm-var ")) {\n"
	    ;; We can't use `scm_i_string_chars ()' here because it returns a
	    ;; non-zero terminated string.  So, no matter whether VALUE is
	    ;; caller-owned or not, we have to allocate a new string a free
	    ;; it afterwards.
	    (if (not inlined?)
		;; allocate a new C string on the heap.
		(list c-var " = scm_to_locale_string ("scm-var");\n")

		;; allocate a new C string on the stack rather.
		(list "{\n"
		      "size_t "c-size-var " = "
		      "scm_c_string_length ("scm-var");\n"
		      c-var " = alloca ("c-size-var" + 1);\n"
		      "scm_to_locale_stringbuf ("scm-var", "c-var",\n"
		      "                         "c-size-var");\n"
		      c-var"["c-size-var"] = '\\0';\n"
		      "}\n"))

	    "} else\n"
	    "{  " c-var " = NULL;\n"
	    `(gw:error ,error-var type ,(wrapped-var value))
	    "\n}\n")))
      (if-typespec-option
       value 'null-ok
       (list "if (SCM_FALSEP(" scm-var "))\n"
	     "  " c-var " = NULL;\n"
	     "else {"
	     unwrap-code
	     "}\n")
       unwrap-code))))

(define-method (destroy-value-cg (type <gw-guile-ctype-mchars>)
				 (value <gw-value>)
				 error-var
				 (inlined? <boolean>))
  ;; `destroy-value-cg' is not called on `out' values that have just been
  ;; passed to `wrap-value-cg', so we don't have to worry about this case.
  (if-typespec-option value 'caller-owned
		      (if (not inlined?)
			  (list "\n{\n"
				"/* Free the string that was allocated "
				"by `scm_to_locale_string ()' \n"
				"   in `unwrap-value'.  */\n"
				"if ("(var value)" != NULL)\n"
				"  free ((char*)"(var value)");\n"
				"}\n")
			  '())
		      '()))
