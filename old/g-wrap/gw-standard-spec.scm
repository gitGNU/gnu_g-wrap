;; -*-scheme-*-

(define-module (g-wrap gw-standard-spec)
  #:use-module (g-wrap)
  #:use-module (g-wrap simple-type)
  #:use-module (g-wrap dynamic-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple ranged integer types.
;;;
;;; code stolen from plain simple-types.  The same, but different :>
  
(define (wrap-simple-ranged-integer-type wrapset
                                         type-sym
                                         c-type-name
                                         c-minval-text ; for unsigned, #f
                                         c-maxval-text
                                         scm->c-function
                                         c->scm-function
                                         c-typedef)

  (let* ((c-sym-name (gw:any-str->c-sym-str (symbol->string type-sym)))
         (minvar (gw:gen-c-tmp (string-append "range_minval" c-sym-name)))
         (maxvar (gw:gen-c-tmp (string-append "range_maxval" c-sym-name))))

    (define (scm->c-ccg c-var scm-var typespec error-var)
      (list "if(SCM_FALSEP(scm_integer_p(" scm-var ")))"
            `(gw:error ,error-var type ,scm-var)
            (if c-minval-text
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
            "  " c-var " = " scm->c-function "(" scm-var ", 0, NULL);\n"  
            "}\n"))
    
    (define (c->scm-ccg scm-var c-var typespec error-var)
      (list scm-var " = " c->scm-function "(" c-var ");\n"))

    (define (c-destructor c-var typespec status-var force?)
      '())

    (define (global-declarations-ccg type client-wrapset)
      (if #t  ;;  (not client-wrapset)<- FIXME: no real need for this in depending wrapsets -- rotty
          (list (if c-minval-text
                    (list "static SCM " minvar ";\n")
                    '())
                "static SCM " maxvar ";\n")
          '()))
    
    ;; TODO: maybe use error-var.
    (define (global-init-ccg type client-wrapset error-var)
      (if #t ;; (not client-wrapset) <- FIXME: no real need forthis in depending wrapsets -- rotty
          (list (if c-minval-text
                    (list minvar " = " c->scm-function "(" c-minval-text ");\n"
                          "scm_gc_protect_object(" minvar ");\n")
                    '())
                maxvar " = " c->scm-function "(" c-maxval-text ");\n"
                "scm_gc_protect_object(" maxvar ");\n")
          '()))
    
    (define (typespec-options-parser options-form wrapset)
      (let ((remainder options-form))
        (set! remainder (delq 'const remainder))
        (if (null? remainder)
            (cons 'callee-owned options-form)
            (throw 'gw:bad-typespec
                   "Bad simple-type options form - spurious options: "
                   remainder))))

    (let ((dynamic-type (gw:wrap-dynamic-type
                         wrapset type-sym c-type-name c-type-name
                         scm->c-ccg c->scm-ccg c-destructor c-typedef)))
    
      (gw:type-set-global-declarations-ccg! dynamic-type global-declarations-ccg)
      (gw:type-set-global-initializations-ccg! dynamic-type global-init-ccg)
      (gw:type-set-typespec-options-parser! dynamic-type typespec-options-parser)
    
    dynamic-type)))

(let ((ws (gw:new-wrapset "gw-standard"))
      (limits-requiring-types '()))

  (gw:wrapset-set-guile-module! ws '(g-wrap gw-standard))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; void
  (let ((wt (gw:wrap-dynamic-type
             ws '<gw:void>
             "void" "void"
             (lambda (c-var scm-var typespec error-var)
               (gw:typespec-check
                typespec
                (error "Can't convert a <gw:void> from Scheme to C.")
                '()))
             (lambda (scm-var c-var typespec error-var)
               (gw:typespec-check
                typespec
                (error "Can't convert a <gw:void> from C to scm.")
                (list scm-var " = SCM_UNSPECIFIED;\n")))
             (lambda (c-var typespec error-var force?)
               (gw:typespec-check typespec
                                  (error "Can't destroy a <gw:void>.")
                                  '()))
             'void)))

    (gw:type-set-typespec-options-parser!
     wt
     (lambda (options-form wrapset)
      (let ((remainder options-form))
        (if (null? remainder)
            (cons 'callee-owned options-form)
            (throw 'gw:bad-typespec
                   "Bad <gw:void> form - spurious options: "
                   remainder)))))

    ;; We overwrite some of the ccgs generated by
    ;; gw:wrap-dynamic-type, so that they don't invoke the ccgs we
    ;; passed it. -- rotty
    (gw:type-set-pre-call-arg-ccg!
     wt
     (lambda (param error-var)
       (error "Can't use <gw:void> as an argument type.")))
    
    (gw:type-set-post-call-result-ccg!
     wt
     (lambda (result error-var)
       (list (gw:result-get-scm-name result) " = SCM_UNSPECIFIED;\n")))

    ;; no result assignment.
    (gw:type-set-call-ccg!
     wt
     (lambda (result func-call-code status-var)
       (list func-call-code ";\n")))

    (gw:type-set-post-call-result-ccg!
     wt
     (lambda (result status-var)
       (list (gw:result-get-scm-name result) " = SCM_UNSPECIFIED;\n")))
    
    (gw:type-declare-scm-result-var?! wt #f))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:scm> - pass scheme pointers through unmolested.
  (gw:wrap-simple-type ws '<gw:scm>
                       "SCM"
                       '("1")
                       '(c-var " = " scm-var ";\n")
                       '(scm-var " = " c-var ";\n")
                       'pointer) ;; FIXME: This is not accurate -- rotty

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:bool> - boolean type
  (gw:wrap-simple-type ws '<gw:bool> "int"
                       ;; Any scheme value is a valid bool.
                       '("1")
                       '(c-var "= SCM_NFALSEP(" scm-var ");\n")
                       '(scm-var "= (" c-var ") ? SCM_BOOL_T : SCM_BOOL_F;\n")
                       'sint)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:char> -- FIXME: scm chars are 0-255, not [-128,127] like c chars
  ;; [rotty: c-chars are not always signed!]
  (gw:wrap-simple-type ws '<gw:char> "char"
                       '("SCM_NFALSEP(scm_char_p(" scm-var "))\n")
                       '(c-var "= SCM_CHAR(" scm-var ");\n")
                       '(scm-var "= SCM_MAKE_CHAR(" c-var ");\n")
                       'schar) ;; FIXME: see above

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:unsigned-char> -- scm chars are bounded to [0,255]
  (gw:wrap-simple-type ws '<gw:unsigned-char> "unsigned char"
                       '("SCM_NFALSEP(scm_char_p(" scm-var "))\n")
                       '(c-var "= SCM_CHAR(" scm-var ");\n")
                       '(scm-var "= SCM_MAKE_CHAR(" c-var ");\n")
                       'uchar)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:float>
  (gw:wrap-simple-type ws '<gw:float> "float"
                       '("SCM_NFALSEP(scm_number_p(" scm-var "))\n")
                       '(c-var "= scm_num2float(" scm-var ", 1,"
                               " \"gw:scm->float\");\n")
                       '(scm-var "= scm_float2num(" c-var ");\n")
                       'float)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:double>
  (gw:wrap-simple-type ws '<gw:double> "double"
                       '("SCM_NFALSEP(scm_number_p(" scm-var "))\n")
                       '(c-var "= scm_num2double(" scm-var ", 1,"
                               " \"gw:scm->double\");\n")
                       '(scm-var "= scm_double2num(" c-var ");\n")
                       'double)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:short>
  (let ((wt (wrap-simple-ranged-integer-type
             ws '<gw:short> "short"
             "SHRT_MIN" "SHRT_MAX"
             "scm_num2short" "scm_short2num"
             'sshort)))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:unsigned-short>
  (let ((wt (wrap-simple-ranged-integer-type
             ws '<gw:unsigned-short> "unsigned short"
             #f "USHRT_MAX"
             "scm_num2ushort" "scm_ushort2num"
             'ushort)))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:int>
  (let ((wt (wrap-simple-ranged-integer-type
             ws '<gw:int> "int"
             "INT_MIN" "INT_MAX"
             "scm_num2int" "scm_int2num"
             'sint)))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:unsigned-int>
  (let ((wt (wrap-simple-ranged-integer-type
             ws '<gw:unsigned-int> "unsigned int"
             #f "UINT_MAX"
             "scm_num2uint" "scm_uint2num"
             'uint)))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:long>
  (let ((wt (wrap-simple-ranged-integer-type
             ws '<gw:long> "long"
             "LONG_MIN" "LONG_MAX"
             "scm_num2long" "scm_long2num"
             'slong)))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:unsigned-long>
  (let ((wt (wrap-simple-ranged-integer-type
             ws '<gw:unsigned-long> "unsigned long"
             #f "ULONG_MAX"
             "scm_num2ulong" "scm_ulong2num"
             'ulong)))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  (if (string>=? (version) "1.6")
      (begin
        ;; There's a bit of a mess in some older guiles wrt long long
        ;; support. I don't know when it was fixed, but I know that the
        ;; 1.6 series works properly -- apw
        ;;
        ;; Maybe we can make Guile 1.6 a requirement -- rotty
        
        ;; FIXME: how to handle the no-long-longs case nicely?
        ;; Why can't an honest guy seem to get a hold of LLONG_MAX?

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; <gw:long-long>
        (let ((wt (wrap-simple-ranged-integer-type
                   ws '<gw:long-long> "long long"
                   "((long long)0x7fffffffffffffffULL)"
                   "((long long)0x8000000000000000ULL)"
                   "scm_num2long_long" "scm_long_long2num"
                   'sint64))) ;; FIXME: not accurate -- rotty
          (set! limits-requiring-types (cons wt limits-requiring-types)))
  
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; <gw:unsigned-long-long>
        (let ((wt (wrap-simple-ranged-integer-type
                   ws '<gw:unsigned-long-long> "unsigned long long"
                   #f "((unsigned long long)0xffffffffffffffffULL)"
                   "scm_num2ulong_long" "scm_ulong_long2num"
                   'uint64))) ;; FIXME: not accurate -- rotty
          (set! limits-requiring-types (cons wt limits-requiring-types)))))



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:mchars>
  (let ()
    ;; This is in fact the same as the dynamic-type one, except that
    ;; we ignore 'null-ok (hack to make guile-gobject work)
    (define (typespec-options-parser options-form wrapset)
      (let ((remainder options-form))
        (set! remainder (delq 'const remainder))
        (set! remainder (delq 'null-ok remainder))
        (if (and (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder))
            (throw 'gw:bad-typespec
                   "Bad <gw:mchars> options form (caller and callee owned!)."
                   options-form))
        (if (not (or (memq 'caller-owned remainder)
                     (memq 'callee-owned remainder)))
            (throw 'gw:bad-typespec
                   (format #t "Bad <gw:mchars> options form for type ~A (must be caller or callee owned!)." type-sym)
                   options-form))
        (set! remainder (delq 'caller-owned remainder))
        (set! remainder (delq 'callee-owned remainder))
        (if (null? remainder)
            options-form
            (throw 'gw:bad-typespec
                   "Bad <gw:mchars> options form - spurious options: "
                   remainder))))
    
    (define (scm->c-ccg c-var scm-var typespec error-var)
      (list
       c-var " = NULL;\n"
       "\n"
       "if(SCM_FALSEP(" scm-var "))\n"
       "  " c-var " = NULL;\n"
       "else if(SCM_STRINGP(" scm-var "))\n"
       "  " c-var " = strdup (SCM_STRING_CHARS (" scm-var "));\n"
       "else\n"
       `(gw:error ,error-var type ,scm-var)))
    
    (define (c->scm-ccg scm-var c-var typespec error-var)
      (list
       "if(" c-var " == NULL) " scm-var " = SCM_BOOL_F;\n"
       "else "
       scm-var " = scm_makfrom0str( " c-var ");\n"))
    
    (define (c-destructor c-var typespec error-var force?)
      (gw:typespec-check
       typespec
       (if (or force?
               (memq 'caller-owned (gw:typespec-get-options typespec)))
           (list "if(" c-var ") free((void *)" c-var ");\n")
           '())
       (list "if (" c-var "&& (" force? "|| (" typespec
             " & GW_TYPESPEC_CALLER_OWNED))) free((void *)" c-var ");\n")))
    
    (let* ((mchars (gw:wrap-dynamic-type ws '<gw:mchars>
                                         "char *" "const char *"
                                         scm->c-ccg c->scm-ccg c-destructor
                                         'pointer)))
    
    (gw:type-set-typespec-options-parser! mchars typespec-options-parser)
    
    mchars))
  
  (gw:wrapset-add-cs-before-includes!
   ws
   (lambda (wrapset client-wrapset)
     (if (and client-wrapset
              (gw:any? (lambda (x) (gw:wrapset-uses-type? client-wrapset x))
                       limits-requiring-types))
         "#define _GNU_SOURCE\n"
         '())))
  
  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      (if (and client-wrapset
               (gw:any? (lambda (x) (gw:wrapset-uses-type? client-wrapset x))
                        limits-requiring-types))
          "#include <limits.h>\n"
          '())
     )))

  )
