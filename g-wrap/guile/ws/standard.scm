;; Copyright (C) 2004 Andreas Rottmann

(define-module (g-wrap guile ws standard)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap ws standard)
  #:use-module (g-wrap guile))
  
(define-class <standard-wrapset> (<gw-guile-wrapset>
                                  <gw-standard-wrapset>)
   #:language guile #:id 'standard)

(define-method (initialize (wrapset <standard-wrapset>) initargs)
  (next-method)

  (set! (module wrapset) '(g-wrap gw standard))
  
  ;; SCM - pass scheme pointers through unmolested.
  (add-type! wrapset
             (make <gw-guile-simple-type>
               #:name 'scm
               #:c-type-name "SCM"
               #:type-check '("1")
               #:ffspec 'pointer ;; FIXME: not accurate
               #:unwrap '(c-var " = " scm-var ";\n")
               #:wrap '(scm-var " = " c-var ";\n")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:wct> - wrapped c pointer type object
  (add-type! wrapset
             (make <gw-guile-simple-type>
               #:name '<gw:wct>
               #:c-type-name "SCM"
               #:type-check '("gw_wct_p(" scm-var ")")
               #:unwrap '(c-var " = " scm-var ";\n")
               #:wrap '(scm-var " = " c-var ";\n")
               #:ffspec 'pointer)) ;; not accurate

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:wcp> - wrapped c pointer object
  (add-type! wrapset
             (make <gw-guile-simple-type>
               #:name '<gw:wcp>
               #:c-type-name "SCM"
               #:type-check '("gw_wcp_p(" scm-var ")")
               #:unwrap '(c-var " = " scm-var ";\n")
               #:wrap '(scm-var " = " c-var ";\n")
               #:ffspec 'pointer)) ;; not accurate

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
                  #:arguments '((scm proc) (scm specializers)
                                (scm generic-name))
                  #:c-name "gw_guile_procedure_to_method_public"))

(define-method (add-type! (wrapset <standard-wrapset>)
                          (type <gw-guile-simple-type>))
  (let ((info (assq-ref
               '((scm) (<gw:wct>) (<gw:wcp>)
                 (bool #f
                       (c-var "= SCM_NFALSEP(" scm-var ");\n")
                       (scm-var "= (" c-var ") ? SCM_BOOL_T : SCM_BOOL_F;\n"))
                 
                 (char ("SCM_NFALSEP(scm_char_p(" scm-var "))")
                       (c-var "= SCM_CHAR(" scm-var ");\n")
                       (scm-var "= SCM_MAKE_CHAR(" c-var ");\n"))
                 
                 (unsigned-char ("SCM_NFALSEP(scm_char_p(" scm-var "))")
                                (c-var "= SCM_CHAR(" scm-var ");\n")
                                (scm-var "= SCM_MAKE_CHAR(" c-var ");\n"))
                 
                 (float ("SCM_NFALSEP(scm_number_p(" scm-var "))")
                        (c-var "= scm_num2float(" scm-var ", 1,"
                               " \"gw:scm->float\");\n")
                        (scm-var "= scm_float2num(" c-var ");\n"))
                 
                 (double ("SCM_NFALSEP(scm_number_p(" scm-var "))\n")
                         (c-var "= scm_num2double(" scm-var ", 1,"
                                " \"gw:scm->double\");\n")
                         (scm-var "= scm_double2num(" c-var ");\n")))
               (name type))))
    (cond ((null? info)
           (next-method))
          ((not info)
           (error "attempt to wrap unknown simple type" (name type)))
          (else
           (slot-set! type 'type-check (list-ref info 0))
           (slot-set! type 'unwrap (list-ref info 1))
           (slot-set! type 'wrap (list-ref info 2))
           (next-method)))))


;;;
;;; <gw-ctype-void>
;;;
(define-method (wrap-value-cg (lang <gw-language>)
                              (type <gw-ctype-void>)
                              (value <gw-value>) error-var)
  (list (scm-var value) " = SCM_UNSPECIFIED;\n"))

(define-method (post-call-result-cg (lang <gw-guile>)
                                    (type <gw-ctype-void>)
                                    (result <gw-value>)
                                    status-var)
  (list (scm-var result) " = SCM_UNSPECIFIED;\n"))

;;;
;;; <ranged-integer-type>
;;;
(define-class <ranged-integer-type> (<gw-ranged-integer-type>)
  (wrap #:init-keyword #:wrap)
  (unwrap #:init-keyword #:unwrap))

(define (ranged-integer-name type)
  (let ((special (assq-ref '((unsigned-short . "ushort")
                             (unsigned-int . "uint")
                             (unsigned-long . "ulong")
                             (long-long . "long_long")
                             (unsigned-long-long . "ulong_long"))
                           (name type))))
    (if special
        special
        (symbol->string (name type)))))

(define-method (wrap-ranged-integer-type! (wrapset <standard-wrapset>) . args)
  (let* ((type (apply make <ranged-integer-type> args))
         (name (ranged-integer-name type)))
    (slot-set! type 'wrap (string-append "scm_" name "2num"))
    (slot-set! type 'unwrap (string-append "scm_num2" name))
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

(define-method (wrap-value-cg (lang <gw-guile>)
                              (type <ranged-integer-type>)
                              (value <gw-value>)
                              error-var)
  (list (scm-var value) " = " (slot-ref type 'wrap) "(" (var value) ");\n"))

(define-method (unwrap-value-cg (lang <gw-guile>)
                                (type <ranged-integer-type>)
                                (value <gw-value>)
                                error-var)
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
          "  " c-var " = " (slot-ref type 'unwrap) "(" scm-var ", 0, NULL);\n"
          "}\n")))


(define-method (global-declarations-cg (lang <gw-guile>)
                                       (wrapset <gw-guile-wrapset>)
                                       (type <ranged-integer-type>))
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
  
(define-method (initializations-cg (lang <gw-guile>)
                                   (wrapset <gw-guile-wrapset>)
                                   (type <ranged-integer-type>)
                                   error-var)
  (list
   (next-method)
   (minmax-var-init-cg type)))

(define-method (client-global-declarations-cg (lang <gw-guile>)
                                              (wrapset <gw-guile-wrapset>)
                                              (type <ranged-integer-type>))
  (global-declarations-cg lang wrapset type))

(define-method (client-initializations-cg (lang <gw-guile>)
                                          (wrapset <gw-guile-wrapset>)
                                          (type <ranged-integer-type>)
                                          error-var)
  (minmax-var-init-cg type))

;;;
;;; <gw-ctype-mchars>
;;;

(define-method (wrap-value-cg (lang <gw-guile>)
                              (type <gw-ctype-mchars>)
                              (value <gw-value>)
                              error-var)
    (list
     "if(" (var value) " == NULL) " (scm-var value) " = SCM_BOOL_F;\n"
     "else "
     (scm-var  value) " = scm_makfrom0str( " (var value) ");\n"))

;; FIMXE: check null-ok
(define-method (unwrap-value-cg (lang <gw-guile>)
                                (type <gw-ctype-mchars>)
                                (value <gw-value>)
                                error-var)
  (let ((c-var (var value))
        (scm-var (scm-var value)))
    (list
     c-var " = NULL;\n"
     "\n"
     "if(SCM_FALSEP(" scm-var "))\n"
     "  " c-var " = NULL;\n"
     "else if(SCM_STRINGP(" scm-var "))\n"
     "  " c-var " = strdup (SCM_STRING_CHARS (" scm-var "));\n"
     "else\n"
     `(gw:error ,error-var type ,(wrapped-var value)))))


(define-method (destruct-value-cg (lang <gw-guile>)
                                  (type <gw-ctype-mchars>)
                                  (value <gw-value>)
                                  error-var)
  (let ((c-var (var value)))
    (if-typespec-option
     value 'caller-owned
     (list "if (" c-var ") free (" c-var ");\n"))))