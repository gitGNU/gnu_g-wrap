;;;; File: guile.scm
;;;; Copyright (C) 2004-2007 Andreas Rottmann
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
; This module extends the G-Wrap core with support for Guile Scheme.
;;
;;; Code:

(define-module (g-wrap guile)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)

  #:use-module (oop goops)
  #:use-module (ice-9 optargs)

  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap enumeration)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap c-codegen)

  #:export (inline-scheme

	    <gw-guile-wrapset>
	    module module-exports
	    add-module-export!

	    <gw-guile-simple-type>
	    <gw-guile-simple-rti-type>
	    <gw-guile-rti-type>
	    scm-var))


;;;
;;; Utilities
;;;

(define (guile-module-name->c-registration-strlist name-symlist)
  (separate-by (map symbol->string name-symlist) " "))

(define (guile-module-name->c-sym-name-strlist name-symlist)
  (separate-by
   (map (lambda (s) (gw:any-str->c-sym-str (symbol->string s)))
	name-symlist)
   "_"))

(define-method (scm-var (value <gw-value>))
  (string-append "*(SCM *)" (wrapped-var value)))

(define-class <gw-guile-function> (<gw-function>)
  wrapper-name wrapper-namestr)

(define-method (initialize (func <gw-guile-function>) initargs)
  (next-method)

  (let ((c-name (c-name func)))
    (slot-set! func 'wrapper-name
	       (gen-c-tmp (string-append c-name "_wrapper")))
    (slot-set! func 'wrapper-namestr
	       (gen-c-tmp (string-append c-name "_namestr")))))



;;;
;;; Wrapset
;;;

(define-class <gw-guile-wrapset> (<gw-rti-wrapset>)
  (module #:init-keyword #:module #:accessor module #:init-value #f)
  (module-exports #:getter module-exports #:init-value '())
  (shlib-path #:init-keyword #:shlib-path)
  (shlib-abs? #:init-keyword #:shlib-abs? #:init-value #f)
  
  #:language 'guile)

(define-method (global-definitions-cg (wrapset <gw-guile-wrapset>))
  (let ((wrapset-name-c-sym (any-str->c-sym-str
			     (symbol->string (name wrapset)))))
    (list
     (next-method)
     "void gw_init_wrapset_" wrapset-name-c-sym "(void);\n"
     "void gw_init_wrapset_" wrapset-name-c-sym "(void)\n"
     "{\n"
     "  gw_initialize_wrapset_" wrapset-name-c-sym "(NULL);\n"
     "}\n")))

(define-method (global-declarations-cg (wrapset <gw-guile-wrapset>))
  (list
   (next-method)
   "#include <g-wrap/guile-runtime.h>\n"))

(define-method (initializations-cg (wrapset <gw-guile-wrapset>) err)
  (list
   "gw_guile_runtime_init ();\n"
   (let ((modules (fold (lambda (ws clauses)
			  (if (module ws)
			      (cons (module ws) clauses)
			      clauses))
			'()
			(wrapsets-depended-on wrapset))))
     (if (null? modules)
	 '()
	 (inline-scheme wrapset `(use-modules ,@modules))))

   (next-method)))

(define-method (initialize (wrapset <gw-guile-wrapset>) initargs)
  (next-method wrapset (append (list #:function-class <gw-guile-function>)
			       initargs))
  (if (not (slot-bound? wrapset 'shlib-path))
      (slot-set! wrapset 'shlib-path
		 (string-append "libgw-guile-"
				(symbol->string (name wrapset))))))

;;; Additional methods

(define-method (add-module-export! (ws <gw-guile-wrapset>) (sym <symbol>))
  (slot-push! ws 'module-exports sym))

(define (scm-form-str->safe-c-str name)
  (define (char->string-replacement char)
    (case char
      ((#\") "\\\"")
      ((#\newline) "\\n")
      (else (string char))))
  (apply
   string-append
   (map
    char->string-replacement
    (string->list name))))

(define-method (inline-scheme (ws <gw-guile-wrapset>) . code-chunks)
  (map
   (lambda (chunk)
     (list "scm_c_eval_string(\""
	   (scm-form-str->safe-c-str
	    (call-with-output-string
	     (lambda (port)
	       (write chunk port))))
	   "\");\n"))
   code-chunks))

;;; Refined methods

(define-method (initializations-cg (wrapset <gw-guile-wrapset>)
				   (constant <gw-constant>) error-var)
  (let* ((scm-var (gen-c-tmp "scm_wrapped_value"))
	 (wrap-value-code
	  (wrap-value-cg (type constant)
			 (make <gw-value>
			   #:var (value constant)
			   #:wrapped-var (string-append "&" scm-var))
			 error-var
			 #t)))
    (list
     "{\n"
     "  SCM " scm-var ";\n"
     "\n"
     wrap-value-code
     "if (!" `(gw:error? ,error-var) ")"
     "  scm_c_define (\"" (symbol->string (name constant)) "\", " scm-var ");\n"
     "}\n")))

(define-method (add-constant! (wrapset <gw-guile-wrapset>)
			      (constant <gw-constant>))
  (next-method)
  (add-module-export! wrapset (name constant)))

(define-method (add-function! (wrapset <gw-guile-wrapset>)
			      (func <gw-function>))

  (next-method)
  (add-module-export! wrapset (name func)))


;;;
;;; Function wrappers
;;;

;; hardcoded here, should be extracted from SCM_SUBR_MAX at configure-time
(define *max-fixed-params* 9)

(define (make-c-wrapper-param-declarations param-list)
  (let loop ((params param-list)
	     (index  0))
    (cond ((null? params)
	   '())
	  ((and (= index *max-fixed-params*))
	   "SCM gw__restargs ")
	  (else
	   (cons
	    (list
	     "SCM " "gw__scm_arg" (number->string index)
	     (if (null? (cdr params))
		 " "
		 ", "))
	    (loop (cdr params) (+ index 1)))))))

(define (make-c-call-param-list params)
  ;; TODO: Make tail-recursive
  (cond ((null? params) '())
	(else
	 (let ((param (car params)))
	   (cons
	    (list
	     (call-arg-cg (type param) param)
	     (if (null? (cdr params))
		 ""
		 ", "))
	    (make-c-call-param-list (cdr params)))))))

(define (actual-arguments function)
  (map
   (lambda (arg number c-number out-number)
     (apply make <gw-param>
	    #:number number
	    #:typespec (typespec arg)
	    #:var (string-append "gw__c_arg" (number->string c-number))
	    (cond
	     ((>= number 0)
		`(#:wrapped-var
		  ,(if (< number *max-fixed-params*)
		       (string-append "&gw__scm_arg" (number->string number))
		       (string-append
			"&gw__scm_extras[" (number->string
					    (- number
					       *max-fixed-params*)) "]"))))
	     ((output-argument? arg)
	      `(#:wrapped-var
		,(string-append "& "(out-param-name out-number))))
	     (else '()))))
   (arguments function)
   (let loop ((numbers '()) (args (arguments function)) (n 0))
     (if (null? args)
	 (reverse numbers)
	 (if (and (visible? (car args))
		  (not (memq 'out (options (typespec (car args))))))
	     (loop (cons n numbers) (cdr args) (+ n 1))
	     (loop (cons -1 numbers) (cdr args) n))))
   (iota (argument-count function))
   (let loop ((numbers '()) (args (arguments function)) (n 0))
     (if (null? args)
	 (reverse numbers)
	 (if (memq 'out (options (typespec (car args))))
	     (loop (cons n numbers) (cdr args) (+ n 1))
	     (loop (cons -1 numbers) (cdr args) n))))
   ))

;; FIXME: This is too unspecialized, so that it is more generic than
;; the method for rti wrapsets. It should be refactored into core.
(define-method (global-definitions-cg (wrapset <gw-guile-wrapset>)
				      (function <gw-guile-function>))
   (if (uses-rti-for-function? wrapset function)
       '()
       (function-wrapper-cg wrapset function)))

(define (out-param-name number)
  (string-append "gw__scm_out_arg" (number->string number)))

(define (function-wrapper-cg wrapset function)
  (let* ((params (actual-arguments function))
	 (c-name (c-name function))
	 (return-typespec (return-typespec function))
	 (return-type (type return-typespec))
	 (result (make <gw-value>
		   #:typespec return-typespec
		   #:wrapped-var "&gw__scm_result"
		   #:var "gw__result"))
	 (scm-params (filter (lambda (param) (and (visible? param)
                                                  (not (output-param? param))))
                             params))

	 (function-has-wcp-args?
	  (or (any (lambda (p) (is-a? (type p) <gw-wct>)) params)
	      (is-a? return-type <gw-wct>)))
	 (function-has-aggregated-args?
	  (any (lambda (p) (memq 'aggregated (options (typespec p))))
	       params))

	 (scheme-sym (symbol->string (name function)))
	 (param-decl (make-c-wrapper-param-declarations scm-params))
	 (fn-c-wrapper (slot-ref function 'wrapper-name))
	 (fn-c-string (slot-ref function 'wrapper-namestr))
	 (nargs (length scm-params))
	 (error-var "gw__error")
	 (labels (make <gw-cs-labels>))
	 (out-params (filter (lambda (param)
			       (memq 'out (options (typespec param))))
			     params)))

    (list
     "static char * " fn-c-string " = \"" scheme-sym "\";\n"
     "static SCM " fn-c-wrapper "  (" param-decl ") {\n"
     (if (and function-has-wcp-args? function-has-aggregated-args?)
	 "  SCM gw__scm_deps = SCM_EOL;\n"
	 (format #f "  /* no WCP arguments: ret=~a */\n"
		 return-type))
     "  SCM gw__scm_result = SCM_UNSPECIFIED;\n"
     "  GWError gw__error = { GW_ERR_NONE, NULL, NULL };\n"
     "  unsigned int gw__arg_pos = 0;\n"
     (if (zero? nargs)
	 '()
	 (list
	  "  const GWTypeSpec *typespec = NULL;\n"
	  "  static const GWTypeSpec typespecs[] = { "
          (string-join
           (map (lambda (param)
                  (typespec-cg (type param) (typespec param)))
                scm-params)
           ", ") " };\n"))

     (if (needs-result-var? return-type)
         (let ((c-value (default-c-value-for-type return-type)))
           (list
            ;; Initialize the C variable if possible.
            (c-type-name return-type return-typespec) " " (var result)
            (if (string? c-value)
                (string-append " = " c-value ";\n")
                ";\n")))
	 '())

     (if (> nargs *max-fixed-params*)
	 (list "  SCM gw__scm_extras[" (- nargs *max-fixed-params*) "];\n")
	 '())

     (filter-map
      (lambda (number out-param)
        (and (visible? out-param)
             (list "  SCM " (out-param-name number) " = SCM_UNSPECIFIED;\n")))
      (iota (length out-params))
      out-params)

     "\n"

      (map
       (lambda (param)
	 (list (c-type-name (type param)) " " (var param)
               (if (output-param? param)
                   ;; Initialize the C variable for the `out' parameter if
                   ;; possible.
                   (let ((c-value
                          (default-c-value-for-type (type param))))
                     (if (string? c-value)
                         (string-append " = " c-value ";\n")
                         ";\n"))
                   ";\n")))
       params)

      (map
       (lambda (param arg)
	 (list
	  (if (and (not (output-param? param)) (visible? param))
	      (list
	       "/* ARG " (number param) " */\n"
	       (if (memq 'aggregated (options (typespec param)))
		   ;; Arguments that are aggregated by a return value are a
		   ;; dependency of this return value and need to be marked
		   ;; as such.
		   (string-append "/* arg " (number->string (number param))
				  " will be aggregated by the result"
				  " (further referred to) */\n"
				  "gw__scm_deps = scm_cons ("
				  (scm-var param) ", gw__scm_deps);\n")
		   "")
	       "typespec = &typespecs[gw__arg_pos];\n"
	       "gw__arg_pos++;\n"
	       (if (>= (number param) *max-fixed-params*)
		   (list
		    "if (SCM_NULLP (gw__restargs))\n"
		    (if (default-value arg)
			(list
			 "  " (scm-var param) "= SCM_UNDEFINED;\n")
			(list
			 "{\n"
			 "  (" error-var ").status = GW_ERR_ARGC;\n"
			 "  " (goto-cg labels
				       (if (zero? (number param))
					   "wrapper_exit"
					   (format #f "post_call_arg_~A"
						   (- (number param) 1))))
			 "}\n"))
		    "else {\n"
		    "  " (scm-var param) " = SCM_CAR (gw__restargs);\n"
		    "    gw__restargs = SCM_CDR (gw__restargs);\n"
		    "}\n")
		   '()))
	      (list "/* ARG " (number param) " (invisible) */\n"))
	  "\n{\n"
	  (if (output-param? param)
	      '()
	      (let ((pre-call-code
		     (expand-special-forms
		      (pre-call-arg-cg (type param) param error-var)
		      param
		      '(memory misc type range arg-type arg-range)
		      #:labels labels)))
		(if (default-value arg)
		    (list
		     "if (scm_is_eq (" (scm-var param) ", SCM_UNDEFINED))\n"
		     "  " (set-value-cg (type arg) param
					(default-value arg))
		     "else {\n"
		     pre-call-code
		     "}\n")
		    pre-call-code)))))
       params (arguments function))

      "if ((" error-var ").status == GW_ERR_NONE)\n"
      "{\n"
      (expand-special-forms
       (pre-call-result-cg return-type result error-var)
       #f '(memory misc type range))

      (let* ((func-call-code
	      (list c-name " (" (make-c-call-param-list params) ")"))
	     (call-code (call-cg return-type result func-call-code
				 error-var)))
	(if (not (no-op? call-code))
	    (list
	     "if ((" error-var ").status != GW_ERR_NONE)"
	     "  " (goto-cg labels
			   (if (zero? nargs)
			       "wrapper_exit"
			       (format #f "post_call_arg_~A" (- nargs 1))))
	     (expand-special-forms call-code #f '(memory misc type range)))
	    "/* no function call requested! */\n"))

      "{\n/* post-call-result-cg */\n"
      (expand-special-forms
       (post-call-result-cg return-type result error-var)
       #f '(memory misc type range))
      "}\n"
      "}\n"

      ;; insert the post-call args code in the opposite order
      ;; of the pre-call code
      (map
       (lambda (param)
	 (list
	  (label-cg labels (format #f "post_call_arg_~A" (number param)))
	  (let ((post-call-code
		 (expand-special-forms
		  (post-call-arg-cg (type param) param error-var)
		  #f '(memory misc type range))))
	    (if (no-op? post-call-code)
		'()
		(list "{\n" post-call-code "}\n")))
	  "}\n"))
       (reverse params))

      (if (and function-has-wcp-args? function-has-aggregated-args?)
	  ;; Since some of the input arguments are aggregated by the output
	  ;; arguments or the return value, iterate over the each output
	  ;; argument and generate code that marks the aggregated objects as
	  ;; its dependencies.  Same for the return value.
	  (append (mark-dependencies-cg (scm-var result))
		  (map (lambda (param)
			 (if-typespec-option param 'out
					     (mark-dependencies-cg
					      (scm-var param))
					     '()))
		       params))
	  '())

      " " (label-cg labels "wrapper_exit")
      "  if(gw__error.status != GW_ERR_NONE)\n"
      "    gw_handle_wrapper_error(NULL, &gw__error,\n"
      "                             " fn-c-string ",\n"
      "                             gw__arg_pos);\n"
      (cond ((null? out-params)
             "  return gw__scm_result;\n")
            ((and (not (needs-result-var? return-type))
                  (null? (cdr out-params)))
             (list "  return (" (out-param-name 0) ");\n"))
            (else
             (list
              "  return scm_values (scm_list_n ("
              (if (needs-result-var? return-type)
                  "gw__scm_result, "
                  '())
              (filter-map (lambda (n out-param)
                            (and (visible? out-param)
                                 (string-append (out-param-name n) ", ")))
                          (iota (length out-params))
                          out-params)
              "SCM_UNDEFINED));\n")))
      "}\n")))

;; RTI functions override this method
(define-method (initializations-cg (wrapset <gw-wrapset>)
				   (function <gw-guile-function>)
				   status-var)

  (let* ((visible-args (input-arguments function))
	 (n-visible-args (length visible-args))
	 (n-optional-visible-args (length (filter identity
						  (map default-value visible-args))))
	 (n-req-args (- n-visible-args n-optional-visible-args))
	 (fn-c-wrapper (slot-ref function 'wrapper-name))
	 (fn-c-string  (slot-ref function 'wrapper-namestr))
	 (flags-c-string (flags function)))
    (list
     "   gw_wrapset_add_function(" (c-info-sym wrapset) ", "
     fn-c-wrapper ", " n-req-args ", " n-optional-visible-args ", "
     "NULL, 0, NULL, NULL, " fn-c-string ", "
     (if (generic-name function)
	 (list "\"" (symbol->string (generic-name function)) "\"")
	 "NULL")
     ", " flags-c-string
     ");\n")))


;;;
;;; RTI
;;;

(define-class <gw-guile-rti-type> (<gw-rti-type>))

;; TODO: We don' get full error messages yet (#f passed as param to
;; expand-special-forms)

(define-method (wrap-value-function-cg (type <gw-guile-rti-type>))
  (let* ((type-name (c-type-name type))
	 (value (make <gw-rti-value>
		  #:var (string-append "(*(" type-name "*)instance)")
		  #:typespec #f ;; the typespec is passed in the function
		  #:wrapped-var "value")))
    (list
     "static void " (wrap-value-function-name type)
     "(GWLangLocative value, GWLangArena arena, const GWTypeSpec *typespec, void *instance, GWError *error) {\n"
     "  "
     ;; Invoke `wrap-value-cg' so that it produces non-inlined code.
     (expand-special-forms (wrap-value-cg type value "*error" #f)
			   #f '(type arg-type range memory misc))
     "}\n")))


(define-method (unwrap-value-function-cg (type <gw-guile-rti-type>))
  (let* ((type-name (c-type-name type))
	 (value (make <gw-rti-value>
		  #:var (string-append "(*(" type-name "*)instance)")
		  #:typespec #f ;; the typespec is passed in the function
		  #:wrapped-var "value")))
    (list
     "static void " (unwrap-value-function-name type)
     "(void *instance, GWLangArena arena, const GWTypeSpec *typespec, GWLangLocative value, GWError *error) {\n"
     "  "
     ;; Invoke `unwrap-value-cg' so that it produces non-inlined code.
     (expand-special-forms (unwrap-value-cg type value "*error" #f)
			   #f '(type arg-type range memory misc))
     "}\n")))

(define-method (destroy-value-function-cg (type <gw-guile-rti-type>))

  (let* ((type-name (c-type-name type))
	 (value (make <gw-rti-value>
		  #:var (string-append "(*(" type-name "*)instance)")
		  #:typespec #f ;; the typespec is passed in the function
		  #:wrapped-var "value")))
    (list
     "static void " (destroy-value-function-name type)
     "(GWLangArena arena, void *instance, const GWTypeSpec *typespec, GWError *error) {\n"
     "  "
     ;; Invoke `destroy-value-cg' so that it produces non-inlined code.
     (expand-special-forms (destroy-value-cg type value "*error" #f)
			   #f '(type arg-type range memory misc))
     "}\n")))

;;;
;;; Enumerations
;;;

(define-class <gw-guile-enum> (<gw-enumeration-type> <gw-guile-rti-type>)
  val->int-c-func
  val->int-scm-func
  val->sym-c-func
  val->sym-scm-func)

(define-method (initialize (enum <gw-guile-enum>) initargs)
  (next-method)
  (let-keywords
   initargs #t ((prefix "enum-"))

   (define (gen-name action) (gen-c-tmp-name enum action)) ;; Just lazy
   (define (enum-prefix)
     (string-append prefix (symbol->string (name enum))))

   (slot-set! enum 'val->int-c-func (gen-name "val_to_int"))
   (slot-set! enum 'val->int-scm-func
	      (string-append (enum-prefix) "-val->int"))

   (slot-set! enum 'val->sym-c-func (gen-name "val_to_sym"))
   (slot-set! enum 'val->sym-scm-func
	      (string-append (enum-prefix) "-val->sym"))))

(define-method (global-definitions-cg (wrapset <gw-guile-wrapset>)
				      (enum <gw-enumeration-type>))
  (list
   (next-method)

   "static SCM " (slot-ref enum 'val->sym-c-func)
   "(SCM gw__scm_val, SCM gw__scm_show_all_p) {\n"
   "  return gw_guile_enum_val2sym(" (val-array-name enum) ", "
   "                               gw__scm_val, gw__scm_show_all_p);\n"
   "}\n"
   "\n"
   "static SCM " (slot-ref enum 'val->int-c-func) "(SCM gw__scm_val) {\n"
   "  return gw_guile_enum_val2int(" (val-array-name enum) ", gw__scm_val);\n"
   "}\n"))

(define-method (initializations-cg (wrapset <gw-guile-wrapset>)
				   (enum <gw-enumeration-type>)
				   error-var)
  (list
   (next-method)

   "scm_c_define_gsubr (\"" (slot-ref enum 'val->int-scm-func) "\", 1, 0, 0,\n"
   "                      " (slot-ref enum 'val->int-c-func) ");\n"
   "scm_c_define_gsubr (\"" (slot-ref enum 'val->sym-scm-func) "\", 2, 0, 0,\n"
   "                      " (slot-ref enum 'val->sym-c-func) ");\n"))

(define-method (wrap-value-cg (type <gw-guile-enum>)
			      (value <gw-value>)
			      status-var
			      (inlined? <boolean>))
  (list (scm-var value) " = scm_long2num(" (var value) ");\n"))

(define-method (unwrap-value-cg (enum <gw-guile-enum>)
				(value <gw-value>)
				status-var
				(inlined? <boolean>))
  (let ((scm-var (scm-var value))
	(c-var (var value))
	(val-sym-array-name (val-array-name enum)))
    (list
     scm-var " = gw_guile_enum_val2int(" val-sym-array-name ", " scm-var ");\n"
     "if(SCM_FALSEP(scm_integer_p(" scm-var ")))"
     `(gw:error ,status-var type ,(wrapped-var value))
     "else " c-var " = scm_num2long(" scm-var
     ", 0, \"%gw:enum->scm->c-ccg\");\n")))

(define-method (wrap-enum! (wrapset <gw-guile-wrapset>) . args)

  (define (slot-sym-ref v slot)
    (string->symbol (slot-ref v slot)))

  (let ((enum (apply make <gw-guile-enum> args)))
    (add-type! wrapset enum)
    (add-module-export! wrapset (slot-sym-ref enum 'val->int-scm-func))
    (add-module-export! wrapset (slot-sym-ref enum 'val->sym-scm-func))
    enum))



;;;
;;; Simple Types
;;;

(define-class <gw-guile-simple-type-base> ()
  (type-check #:init-keyword #:type-check)
  (wrap #:init-keyword #:wrap)
  (unwrap #:init-keyword #:unwrap))

;; Helper
(define (replace-syms tree alist)
  (cond
   ((null? tree) tree)
   ((list? tree) (map (lambda (elt) (replace-syms elt alist)) tree))
   ((symbol? tree)
    (let ((expansion (assq-ref alist tree)))
      (if (string? expansion)
	  expansion
	  (error
	   (string-append
	    "g-wrap expected string for expansion "
	    "while processing  ~S\n.") expansion))))
   (else tree)))

(define-method (unwrap-value-cg (type <gw-guile-simple-type-base>)
				(value <gw-value>)
				status-var
				(inlined? <boolean>))
  (let* ((scm-var (scm-var value))
	 (c-var (var value))
	 (unwrap-code (replace-syms (slot-ref type 'unwrap)
				    `((c-var . ,c-var)
				      (scm-var . ,scm-var))))
	 (type-check (slot-ref type 'type-check)))
    (if type-check
	(list "if (!(" (replace-syms type-check `((scm-var . ,scm-var))) "))"
	      `(gw:error ,status-var type ,(wrapped-var value))
	      "else {" unwrap-code "}")
	unwrap-code)))

(define-method (wrap-value-cg (type <gw-guile-simple-type-base>)
			      (value <gw-value>)
			      status-var
			      (inlined? <boolean>))
  (replace-syms (slot-ref type 'wrap)
		`((c-var . ,(var value))
		  (scm-var . ,(scm-var value)))))

(define-class <gw-guile-simple-rti-type> (<gw-guile-simple-type-base>
                                          <gw-simple-rti-type>
					  <gw-guile-rti-type>))

;; This class is mainly for compatibility: it's like
;; <gw-guile-simple-rti-type>, but doesn't require an ffspec
(define-class <gw-guile-simple-type> (<gw-guile-simple-type-base> <gw-type>)
  (c-type-name #:getter c-type-name #:init-keyword #:c-type-name)
  (c-const-type-name #:init-keyword #:c-const-type-name))

(define-method (c-type-name (type <gw-guile-simple-type>)
			    (typespec <gw-typespec>))
  (slot-ref type (if (memq 'const (options typespec))
		     'c-const-type-name
		     'c-type-name)))

(define-method (wrap-simple-type! (wrapset <gw-guile-wrapset>) . args)
  (let* ((class (if (member #:ffspec args)
		   <gw-guile-simple-rti-type>
		   <gw-guile-simple-type>))
	 (type (apply make class args)))
    (add-type! wrapset type)
    type))


;;;
;;; Wrapped C Types
;;;

(define-class <gw-guile-wct> (<gw-wct> <gw-guile-rti-type>)
  wct-var-name)

(define-method (wrap-as-wct! (wrapset <gw-guile-wrapset>) . args)
  (let ((type (apply make <gw-guile-wct> args)))
    (add-module-export! wrapset (name type))
    (add-type! wrapset type)
    type))

(define-method (initialize (wct <gw-guile-wct>) initargs)
  (next-method)
  (slot-set! wct 'wct-var-name
	     (gen-c-tmp (string-append
			 "wct_info_for"
			 (any-str->c-sym-str (symbol->string (name wct)))))))

(define-method (wrap-value-cg (wct <gw-guile-wct>)
			      (value <gw-value>)
			      status-var
			      (inlined? <boolean>))
  (let ((wct-var (slot-ref wct 'wct-var-name))
	(sv (scm-var value))
	(cv (var value)))
    (list
     "if(" cv " == NULL) " sv " = SCM_BOOL_F;\n"
     "else {\n"
     sv " = gw_wcp_assimilate_ptr((void *) " cv ", " wct-var ");\n"
     "}\n")))


(define-method (unwrap-value-cg (wct <gw-guile-wct>)
				(value <gw-value>)
				status-var
				(inlined? <boolean>))
  (let* ((wct-var (slot-ref wct 'wct-var-name))
	 (sv (scm-var value))
	 (c-var (var value))
	 (unwrap-code
	  (list "if (gw_wcp_is_of_type_p (" wct-var ", " sv "))\n"
		"  " c-var " = gw_wcp_get_ptr (" sv ");\n"
		"else\n"
		`(gw:error ,status-var type ,(wrapped-var value)))))

    (list
     (if-typespec-option value 'null-ok
			 (list "if (SCM_FALSEP (" sv "))\n"
			       "  " c-var " = NULL;\n"
			       "else " unwrap-code)
			 unwrap-code))))


(define (mark-dependencies-cg out-scm-var)
  ;; Set the dependencies of OUT-SCM-VAR, a WCP being returned.
  (list "\n{\n"
	"if ((gw__scm_deps != SCM_EOL) && (SCM_NIMP ("out-scm-var")))\n"
	"  gw_wcp_set_dependencies (" out-scm-var ", "
	"gw__scm_deps);\n"
	"}\n"))

(define-method (post-call-result-cg (return-type <gw-guile-wct>)
				    (result <gw-value>) error-var)
  (next-method))

(define-method (post-call-arg-cg (arg-type <gw-guile-wct>)
				 (arg <gw-value>) error-var)
  (next-method))


(define-method (initializations-cg (wrapset <gw-wrapset>)
				   (wct <gw-guile-wct>)
				   error-var)
  (let ((wct-var (slot-ref wct 'wct-var-name))
	(wcp-type-name (symbol->string (name wct)))
	(wcp-mark (wcp-mark-function wct))
	(wcp-free (wcp-free-function wct))
	(wcp-equal? (wcp-equal-predicate wct)))
  (list
   (next-method)

   wct-var "= gw_wct_create (\"" wcp-type-name "\", " wcp-equal? ", NULL, "
   wcp-mark ", " wcp-free ");\n"
   "scm_c_define(\"" wcp-type-name "\", " wct-var ");\n")))

(define (wct-var-decl-cg wct)
  (list "static SCM "  (slot-ref wct 'wct-var-name) " = SCM_BOOL_F;\n"))

(define-method (global-declarations-cg (wrapset <gw-wrapset>)
				       (wct <gw-guile-wct>))
  (wct-var-decl-cg wct))

(define-method (client-global-declarations-cg (wrapset <gw-wrapset>)
					      (wct <gw-guile-wct>))
  (wct-var-decl-cg wct))

(define-method (client-initializations-cg (wrapset <gw-wrapset>)
					  (wct <gw-guile-wct>)
					  error-var)
  (let ((wct-var (slot-ref wct 'wct-var-name))
	(wcp-type-name (symbol->string (name wct))))
    (list
     "    " wct-var " = scm_c_eval_string(\"" wcp-type-name "\");\n")))


;;;
;;; Generation
;;;

(define (generate-wrapset-scm wrapset port)
  (define (dsp-list lst)
    (for-each (lambda (s) (display s port)) lst))

  (let* ((wrapset-name (name wrapset))
	 (wrapset-name-c-sym (any-str->c-sym-str
			      (symbol->string wrapset-name)))
	 (guile-module (module wrapset)))

    (flatten-display
     (list
      ";; Generated by G-Wrap-TNG: an experimental Guile C API-wrapper engine.\n"
      "\n"
      (format #f "(define-module ~S\n" guile-module)
      (format #f "  #:use-module (oop goops)\n")
      (format #f "  #:use-module (g-wrap config)\n")
      ")\n"
      "\n"
      (if (slot-ref wrapset 'shlib-abs?)
          (list "(dynamic-call \"gw_init_wrapset_" wrapset-name-c-sym "\"\n"
                "              (dynamic-link (string-append *g-wrap-shlib-dir* \""
                (slot-ref wrapset 'shlib-path) "\")))\n")
          (list "(dynamic-call \"gw_init_wrapset_" wrapset-name-c-sym "\"\n"
                "              (dynamic-link \"" (slot-ref wrapset 'shlib-path) "\"))\n"))
      "(export " (map (lambda (sym)
			(list "    " sym "\n"))
		      (module-exports wrapset))
      ")"
;;      "(module-use! (module-public-interface (current-module)) (current-module))\n"
      )
     port)
    (let ((gf-hash (make-hash-table 67)))
      (fold-functions
       (lambda (func rest)
	 (let ((gf-name (generic-name func)))
	   (if (and gf-name
		    (> (argument-count func) 0)
		    (class-name (first (argument-types func)))
		    (not (uses-rti-for-function? wrapset func)))
	       (let ((handle
		      (hashq-create-handle! gf-hash gf-name '())))
		 (set-cdr! handle (cons func (cdr handle)))))))
       #f wrapset)
      (hash-fold
       (lambda (gf funcs rest)
         (for-each
          (lambda (func)
            (write
             `(%gw:procedure->method-public
               ,(name func)
               ;; Specializers
               ',(map (lambda (arg)
                        (let ((typespec (typespec arg)))
                          (and (not (memq 'unspecialized
                                          (options typespec)))
                               (class-name (type typespec)))))
                      (filter visible? (arguments func)))
               ',gf
               ;; Required argument count
               ,(- (input-argument-count func)
                   (optional-argument-count func))
               ;; Optional arguments?
               ,(not (zero? (optional-argument-count func))))
             port)
            (newline port))
          funcs)
         (newline port))
       #f gf-hash)
      (write
       '(if (defined? '%generics)
            (begin
              (module-use! (module-public-interface (current-module))
                           %generics)))
       port)
      (newline port))))

(define (make-header-def-sym filename)
  (string-append "__"
		 (string-map
		  (lambda (ch)
		    (case ch
		      ((#\space #\. #\-) #\_)
		      (else ch)))
		  (string-upcase filename))
		 "__"))

(define (generate-wrapset-h wrapset port)
  (let* ((wrapset-name (symbol->string (name wrapset)))
	 (wrapset-header-name (string-append wrapset-name ".h"))
	 (wrapset-name-c-sym (any-str->c-sym-str wrapset-name))
	 (wrapset-header-def-sym (make-header-def-sym wrapset-header-name)))
    (flatten-display
     (list
      "/* Generated by G-Wrap: an experimental C->Guile wrapper engine */\n"
      "\n"
      "#ifndef " wrapset-header-def-sym "\n"
      "#define " wrapset-header-def-sym "\n"
      "\n"
      "#ifdef __cplusplus\n"
      "extern \"C\" {\n"
      "#endif\n"
      "\n"
      "void gw_init_wrapset_" wrapset-name-c-sym "(void);\n"
      "\n"
      "#ifdef __cplusplus\n"
      "}\n"
      "#endif\n"
      "#endif\n")
     port)))

(define-method (generate-wrapset (lang <symbol>)
				 (wrapset <gw-guile-wrapset>)
				 (basename <string>))
  (next-method)

  (call-with-output-file/cleanup
   (string-append basename ".h")
   (lambda (port)
     (generate-wrapset-h wrapset port)))

  (if (module wrapset)
      (call-with-output-file/cleanup
       (string-append basename ".scm")
       (lambda (port)
	   (generate-wrapset-scm wrapset port)))))
