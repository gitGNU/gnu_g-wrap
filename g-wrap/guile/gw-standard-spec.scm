;; Copyright (C) 2004 Andreas Rottmann

(define-module (g-wrap guile gw-standard-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  #:use-module (g-wrap ffi)
  #:use-module (g-wrap guile)
  
  #:export (wrapset-gw-standard))

(define-class <gw-standard-wrapset> (<gw-guile-wrapset>)
  (use-limits? #:init-value #f))

(define-method (initialize (wrapset <gw-standard-wrapset>) initargs)
  (next-method)
  ;; FIXME: This does not consider client wrapsets yet
  (add-cs-before-includes! wrapset 
                           (lambda (lang)
                             (if (slot-ref wrapset 'use-limits?)
                                 (list "#define _GNU_SOURCE\n")
                                 '())))
  (add-cs-global-declarator!
   wrapset
   (lambda (lang)
     (list
      (if (slot-ref wrapset 'use-limits?)
          (list "#include <limits.h>\n")
          '())
      "#include <string.h>\n"))))

(define wrapset (make <gw-standard-wrapset>
                  #:name "gw-standard"
                  #:module '(g-wrap gw-standard)))

(define wrapset-gw-standard wrapset)

;;
;; <simple-ranged-integer-type>
;;
(define-class <simple-ranged-integer-type> (<gw-ffi-type>)
  (min #:init-keyword #:min #:init-value #f)
  (max #:init-keyword #:max)
  (wrap #:init-keyword #:wrap)
  (unwrap #:init-keyword #:unwrap)
  min-var
  max-var)

(define-method (make-typespec (type <simple-ranged-integer-type>)
                              (options <list>))
  (if (null? options)
      (make <gw-typespec> #:type type)
      (throw
       'gw:bad-typespec #f
       (format #f "bad <simple-ranged-integer-type> typespec ~S" options))))

(define-method (add-type! (wrapset <gw-standard-wrapset>)
                          (type <simple-ranged-integer-type>))
  (next-method)
  (slot-set! wrapset 'use-limits? #t))

(define-method (initialize (type <simple-ranged-integer-type>) initargs)
  (next-method)
  (let ((c-sym-name (any-str->c-sym-str (c-type-name type))))
    (slot-set! type 'min-var
               (gen-c-tmp (string-append "range_minval" c-sym-name)))
    (slot-set! type 'max-var
               (gen-c-tmp (string-append "range_minval" c-sym-name)))))

(define-method (wrap-value-cg (lang <gw-guile>)
                              (type <simple-ranged-integer-type>)
                              (value <gw-value>)
                              error-var)
  (list (wrapped-var value) " = "
        (slot-ref type 'wrap) "(" (var value) ");\n"))

(define-method (unwrap-value-cg (lang <gw-guile>)
                                (type <simple-ranged-integer-type>)
                                (value <gw-value>)
                                error-var)
  (let ((scm-var (wrapped-var value))
        (c-var (var value))
        (minvar (slot-ref type 'min-var))
        (maxvar (slot-ref type 'max-var)))
    (list "if(SCM_FALSEP(scm_integer_p(" scm-var ")))"
          `(gw:error ,error-var type ,scm-var)
          (if (slot-ref type 'min)
              (list
               "else if(SCM_FALSEP(scm_geq_p(" scm-var ", " minvar "))"
               "        || SCM_FALSEP(scm_leq_p(" scm-var ", " maxvar ")))")
              (list
               "else if(SCM_NFALSEP(scm_negative_p(" scm-var "))"
               "        || SCM_FALSEP(scm_leq_p(" scm-var ", " maxvar ")))"))
          `(gw:error ,error-var range ,scm-var)
          "else {\n"
          ;; here we pass NULL and 0 as the callers because we've already
          ;; checked the bounds on the argument
          "  " c-var " = " (slot-ref type 'unwrap) "(" scm-var ", 0, NULL);\n"
          "}\n")))


(define-method (global-declarations-cg (lang <gw-guile>)
                                       (wrapset <gw-guile-wrapset>)
                                       (type <simple-ranged-integer-type>))
  (list
   (next-method)
   (if (slot-ref type 'min)
       (list "static SCM " (slot-ref type 'min-var) ";\n")
       '())
   "static SCM " (slot-ref type 'max-var) ";\n"))

(define-method (initializations-cg (lang <gw-guile>)
                                   (wrapset <gw-guile-wrapset>)
                                   (type <simple-ranged-integer-type>)
                                   error-var)
  (let ((minvar (slot-ref type 'min-var))
        (maxvar (slot-ref type 'max-var))
        (minval (slot-ref type 'min))
        (maxval (slot-ref type 'max)))
    (list
     (next-method)
     (if minval
         (list minvar " = " (slot-ref type 'wrap) "(" minval ");\n"
               "scm_gc_protect_object(" minvar ");\n")
         '())
     maxvar " = " (slot-ref type 'wrap) "(" maxval ");\n"
     "scm_gc_protect_object(" maxvar ");\n")))

                            
(define <simple-type> <gw-guile-simple-type>) ;; We're lazy

 ;; void is class of its, own, of course ;-)
(define-class <void> (<gw-ffi-type>))

(define-method (make-typespec (type <void>) (options <list>))
  (if (null? options)
      (make <gw-typespec> #:type type)
      (throw 'gw:bad-typespec
             "Bad <void> options - spurious options: " options)))

;; Only invoked thru ffi
(define-method (wrap-value-cg (lang <gw-language>)
                              (type <void>)
                              (value <gw-value>) error-var)
  '()) 

(define-method (unwrap-value-cg (lang <gw-language>)
                                (type <void>)
                                (value <gw-value>) error-var)
  '()) 

(define-method (pre-call-arg-cg (lang <gw-language>)
                                (type <void>)
                                (param <gw-value>)
                                status-var)
  (error "Can't use void as an argument type."))

(define-method (post-call-arg-cg (lang <gw-language>)
                                (type <void>)
                                (param <gw-value>)
                                status-var)
  (error "Can't use void as an argument type."))

;; no result assignment.
(define-method (call-ccg (lang <gw-language>)
                         (type <void>)
                         (result <gw-value>)
                         (func-call-code <gw-code>)
                         status-var)
  (list func-call-code ";\n"))

(define-method (post-call-result-cg (lang <gw-guile>)
                                    (type <void>)
                                    (result <gw-value>)
                                    status-var)
  (list (wrapped-var result) " = SCM_UNSPECIFIED"))

(add-type! wrapset
           (make <void>
             #:name 'void
             #:c-type-name "void"
             #:ffspec 'void))
 
;; SCM - pass scheme pointers through unmolested.
(add-type! wrapset
           (make <simple-type>
             #:name 'scm
             #:c-type-name "SCM"
             #:type-check '("1")
             #:ffspec 'pointer ;; FIXME: not accurate
             #:unwrap '(c-var " = " scm-var ";\n")
             #:wrap '(scm-var " = " c-var ";\n")))

(add-type! wrapset
           (make <simple-type>
             #:name 'bool
             #:c-type-name "int"
             #:type-check '("1")  ;; Any scheme value is a valid bool.
             #:ffspec 'sint
             #:unwrap '(c-var "= SCM_NFALSEP(" scm-var ");\n")
             #:wrap '(scm-var "= (" c-var ") ? SCM_BOOL_T : SCM_BOOL_F;\n")))

;; FIXME: Guile chars are 0-255, not [-128,127] like c chars *may* be
(add-type! wrapset
           (make <simple-type>
             #:name 'char
             #:c-type-name "char"
             #:type-check '("SCM_NFALSEP(scm_char_p(" scm-var "))\n")
             #:unwrap '(c-var "= SCM_CHAR(" scm-var ");\n")
             #:wrap '(scm-var "= SCM_MAKE_CHAR(" c-var ");\n")
             #:ffspec 'schar)) ;; FIXME: see above

(add-type! wrapset
           (make <simple-type>
             #:name 'uchar
             #:c-type-name "unsigned char"
             #:type-check '("SCM_NFALSEP(scm_char_p(" scm-var "))\n")
             #:unwrap '(c-var "= SCM_CHAR(" scm-var ");\n")
             #:wrap '(scm-var "= SCM_MAKE_CHAR(" c-var ");\n")
             #:ffspec 'uchar))

(add-type! wrapset
           (make <simple-type>
             #:name 'float
             #:c-type-name "float"
             #:type-check '("SCM_NFALSEP(scm_number_p(" scm-var "))\n")
             #:unwrap '(c-var "= scm_num2float(" scm-var ", 1,"
                              " \"gw:scm->float\");\n")
             #:wrap '(scm-var "= scm_float2num(" c-var ");\n")
             #:ffspec 'float))

(add-type! wrapset
           (make <simple-type>
             #:name 'double
             #:c-type-name "double"
             #:type-check '("SCM_NFALSEP(scm_number_p(" scm-var "))\n")
             #:unwrap '(c-var "= scm_num2double(" scm-var ", 1,"
                              " \"gw:scm->double\");\n")
             #:wrap '(scm-var "= scm_double2num(" c-var ");\n")
             #:ffspec 'double))

;; We might well do these with a loop iteration, since the types are
;; quite regular
(add-type! wrapset
           (make <simple-ranged-integer-type>
             #:name 'short
             #:c-type-name "short"
             #:min "SHRT_MIN" #:max "SHRT_MAX"
             #:wrap "scm_short2num" #:unwrap "scm_num2short"
             #:ffspec 'sshort))

(add-type! wrapset 
           (make <simple-ranged-integer-type>
             #:name 'unsigned-short
             #:c-type-name "unsigned short"
             #:max "USHRT_MAX"
             #:wrap "scm_ushort2num" #:unwrap "scm_num2ushort"
             #:ffspec 'ushort))

(add-type! wrapset
           (make <simple-ranged-integer-type>
             #:name 'int
             #:c-type-name "int"
             #:min "INT_MIN" #:max "INT_MAX"
             #:wrap "scm_int2num" #:unwrap "scm_num2int"
             #:ffspec 'sint))

(add-type! wrapset
           (make <simple-ranged-integer-type>
             #:name 'unsigned-int
             #:c-type-name "unsigned int"
             #:max "UINT_MAX"
             #:wrap "scm_uint2num" #:unwrap "scm_num2uint"
             #:ffspec 'uint))

(add-type! wrapset
           (make <simple-ranged-integer-type>
             #:name  'long
             #:c-type-name "long"
             #:min "LONG_MIN" #:max "LONG_MAX"
             #:wrap "scm_long2num" #:unwrap "scm_num2long"
             #:ffspec 'slong))

(add-type! wrapset
           (make <simple-ranged-integer-type>
             #:name 'unsigned-long
             #:c-type-name "long"
             #:min "LONG_MIN" #:max "LONG_MAX"
             #:wrap "scm_long2num" #:unwrap "scm_num2long"
             #:ffspec 'slong))

(add-type! wrapset
           (make <simple-ranged-integer-type>
             #:name 'long-long
             #:c-type-name "long long"
             #:min "LLONG_MIN" #:max "LLONG_MAX"
             #:wrap "scm_long_long2num" #:unwrap "scm_num2long_long"
             #:ffspec 'slong_long))

(add-type! wrapset
           (make <simple-ranged-integer-type>
             #:name  'unsigned-long-long
             #:c-type-name "unsigned long long"
             #:max "ULLONG_MAX"
             #:wrap "scm_ulong_long2num" #:unwrap "scm_num2ulong_long"
             #:ffspec 'ulong_long))


(define-class <mchars> (<gw-ffi-type>))

(define-method (wrap-value-cg (lang <gw-guile>)
                              (type <mchars>)
                              (value <gw-value>)
                              error-var)
  (let ((c-var (var value))
        (scm-var (wrapped-var value)))
    (list
     "if(" c-var " == NULL) " scm-var " = SCM_BOOL_F;\n"
     "else "
     scm-var " = scm_makfrom0str( " c-var ");\n")))

(define-method (unwrap-value-cg (lang <gw-guile>)
                                (type <mchars>)
                                (value <gw-value>)
                                error-var)
  (let ((c-var (var value))
        (scm-var (wrapped-var value)))
    (list
     c-var " = NULL;\n"
     "\n"
     "if(SCM_FALSEP(" scm-var "))\n"
     "  " c-var " = NULL;\n"
     "else if(SCM_STRINGP(" scm-var "))\n"
     "  " c-var " = strdup (SCM_STRING_CHARS (" scm-var "));\n"
     "else\n"
     `(gw:error ,error-var type ,scm-var))))


(define-method (destruct-value-cg (lang <gw-guile>)
                                  (type <mchars>)
                                  (value <gw-value>)
                                  error-var)
  (let ((c-var (var value)))
     ;; FIXME: typespec-option (caller-owned)
    (list "if (" c-var ") free (" c-var ");\n")))

(add-type! wrapset
           (make <mchars>
             #:name 'mchars
             #:c-type-name "char *" #:c-const-type-name "const char *"
             #:ffspec 'pointer))
