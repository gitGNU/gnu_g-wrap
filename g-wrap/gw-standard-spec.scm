;; -*-scheme-*-

(define-module (g-wrap gw-standard-spec))

(use-modules (g-wrap))
(use-modules (g-wrap simple-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple ranged integer types.
;;;
;;; code stolen from plain simple-types.  The same, but different :>
  
(define (wrap-simple-ranged-signed-integer-type wrapset
                                                type-sym
                                                c-type-name
                                                scm-minval-text
                                                scm-maxval-text
                                                scm->c-form
                                                c->scm-form)
  
  (define (replace-syms tree alist)
    (cond
     ((null? tree) tree)
     ((list? tree) (map (lambda (elt) (replace-syms elt alist)) tree))
     ((symbol? tree)
      (let ((expansion (assq-ref alist tree)))
        (if (string? expansion)
            expansion
            (error "Expected string for expansion..."))))
     (else tree)))
  
  (let* ((simple-type (gw:wrap-type wrapset type-sym))
         (c-sym-name (gw:any-str->c-sym-str (symbol->string type-sym)))
         (minvar (gw:gen-c-tmp (string-append "range_minval" c-sym-name)))
         (maxvar (gw:gen-c-tmp (string-append "range_maxval" c-sym-name))))
    
    (define (c-type-name-func typespec)
      c-type-name)
    
    (define (global-declarations-ccg type client-wrapset)
      (if client-wrapset
          (list "static SCM " minvar ";\n"
                "static SCM " maxvar ";\n")
          '()))
    
    ;; TODO: maybe use status-var.
    (define (global-init-ccg type client-wrapset status-var)
      (if client-wrapset
          (list minvar " = " scm-minval-text ";\n"
                "scm_gc_protect_object(" minvar ");\n"
                maxvar " = " scm-maxval-text ";\n"
                "scm_gc_protect_object(" maxvar ");\n")
          '()))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (let ((scm->c-code (replace-syms scm->c-form `((c-var . ,c-var)
                                                     (scm-var . ,scm-var)))))
        (list "if(SCM_FALSEP(scm_integer_p(" scm-var ")))"
              `(gw:error ,status-var type ,scm-var)
              "else if(SCM_FALSEP(scm_geq_p(" scm-var ", " minvar "))"
              "        || SCM_FALSEP(scm_leq_p(" scm-var ", " maxvar ")))"
              `(gw:error ,status-var range ,scm-var)
              "else {" scm->c-code "}\n"
              "\n"
              "if(" `(gw:error? ,status-var type) ")"
              `(gw:error ,status-var arg-type)
              "else if(" `(gw:error? ,status-var range) ")"
              `(gw:error ,status-var arg-range))))
              
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (replace-syms c->scm-form
                    `((c-var . ,c-var)
                      (scm-var . ,scm-var))))
    
    (define (pre-call-arg-ccg param status-var)
      (let* ((scm-name (gw:param-get-scm-name param))
             (c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (list
         (scm->c-ccg c-name scm-name typespec status-var)
         "if(" `(gw:error? ,status-var type) ")"
         `(gw:error ,status-var arg-type)
         "else if(" `(gw:error? ,status-var range) ")"
         `(gw:error ,status-var arg-range))))


    (define (call-ccg result func-call-code status-var)
      (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
    
    (define (post-call-result-ccg result status-var)
      (let* ((scm-name (gw:result-get-scm-name result))
             (c-name (gw:result-get-c-name result))
             (typespec (gw:result-get-typespec result)))
        (c->scm-ccg scm-name c-name typespec status-var)))
    
    (gw:type-set-c-type-name-func! simple-type c-type-name-func)
    (gw:type-set-global-declarations-ccg! simple-type global-declarations-ccg)
    (gw:type-set-global-initializations-ccg! simple-type global-init-ccg)
    (gw:type-set-scm->c-ccg! simple-type scm->c-ccg)
    (gw:type-set-c->scm-ccg! simple-type c->scm-ccg)
    (gw:type-set-pre-call-arg-ccg! simple-type pre-call-arg-ccg)
    (gw:type-set-call-ccg! simple-type call-ccg)
    (gw:type-set-post-call-result-ccg! simple-type post-call-result-ccg)
    
    simple-type))

(define (wrap-simple-ranged-unsigned-integer-type wrapset
                                                  type-sym
                                                  c-type-name
                                                  scm-maxval-text
                                                  scm->c-form
                                                  c->scm-form)
  
  (define (replace-syms tree alist)
    (cond
     ((null? tree) tree)
     ((list? tree) (map (lambda (elt) (replace-syms elt alist)) tree))
     ((symbol? tree)
      (let ((expansion (assq-ref alist tree)))
        (if (string? expansion)
            expansion
            (error "Expected string for expansion..."))))
     (else tree)))
  
  (let* ((simple-type (gw:wrap-type wrapset type-sym))
         (c-sym-name (gw:any-str->c-sym-str (symbol->string type-sym)))
         (maxvar (gw:gen-c-tmp (string-append "range_maxval" c-sym-name))))
    
    (define (c-type-name-func typespec)
      c-type-name)
    
    (define (global-declarations-ccg type client-wrapset)
      (if client-wrapset
          (list "static SCM " maxvar ";\n")
          '()))

    ;; TODO: maybe use status-var
    (define (global-init-ccg type client-wrapset status-var)
      (if client-wrapset
          (list maxvar " = " scm-maxval-text ";\n"
                "scm_gc_protect_object(" maxvar ");\n")
          '()))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (let ((scm->c-code (replace-syms scm->c-form `((c-var . ,c-var)
                                                     (scm-var . ,scm-var)))))
        
        (list
         "if(SCM_FALSEP(scm_integer_p(" scm-var ")))"
         `(gw:error ,status-var type ,scm-var)
         "else if(SCM_NFALSEP(scm_negative_p(" scm-var "))"
         "        || SCM_FALSEP(scm_leq_p(" scm-var ", " maxvar ")))"
         `(gw:error ,status-var range ,scm-var)
         "else {" scm->c-code "}\n")))
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (replace-syms c->scm-form
                    `((c-var . ,c-var)
                      (scm-var . ,scm-var))))
    
    (define (pre-call-arg-ccg param status-var)
      (let* ((scm-name (gw:param-get-scm-name param))
             (c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (list
         (scm->c-ccg c-name scm-name typespec status-var)
         "if(" `(gw:error? ,status-var type) ")"
         `(gw:error ,status-var arg-type)
         "else if(" `(gw:error? ,status-var range) ")"
         `(gw:error ,status-var arg-range))))
    
    (define (call-ccg result func-call-code status-var)
      (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
    
    (define (post-call-result-ccg result status-var)
      (let* ((scm-name (gw:result-get-scm-name result))
             (c-name (gw:result-get-c-name result))
             (typespec (gw:result-get-typespec result)))
        (c->scm-ccg scm-name c-name typespec status-var)))
    
    (gw:type-set-c-type-name-func! simple-type c-type-name-func)
    (gw:type-set-global-declarations-ccg! simple-type global-declarations-ccg)
    (gw:type-set-global-initializations-ccg! simple-type global-init-ccg)
    (gw:type-set-scm->c-ccg! simple-type scm->c-ccg)
    (gw:type-set-c->scm-ccg! simple-type c->scm-ccg)
    (gw:type-set-pre-call-arg-ccg! simple-type pre-call-arg-ccg)
    (gw:type-set-call-ccg! simple-type call-ccg)
    (gw:type-set-post-call-result-ccg! simple-type post-call-result-ccg)
    
    simple-type))


(let ((ws (gw:new-wrapset "gw-standard"))
      (limits-requiring-types '()))

  (gw:wrapset-set-guile-module! ws '(g-wrap gw-standard))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; void
  (let ((wt (gw:wrap-type ws '<gw:void>)))

    (gw:type-set-c-type-name-func!
     wt
     (lambda (typespec) "void"))

    (gw:type-set-scm->c-ccg!
     wt
     (lambda (c-var scm-var typespec status-var)
       (error "Can't convert a <gw:void> from Scheme to C.")))

    (gw:type-set-c->scm-ccg!
     wt
     (lambda (scm-var c-var typespec status-var)
       (error "Can't convert a <gw:void> from C to scm.")))

    (gw:type-set-c-destructor!
     wt
     (lambda (c-var typespec status-var force?)
       (error "Can't destroy a <gw:void>.")))

    (gw:type-set-pre-call-arg-ccg!
     wt
     (lambda (param status-var)
       (error "Can't use <gw:void> as an argument type.")))
    
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
                       '(scm-var " = " c-var ";\n"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:bool> - boolean type
  (gw:wrap-simple-type ws '<gw:bool> "int"
                       ;; Any scheme value is a valid bool.
                       '("1")
                       '(c-var "= SCM_NFALSEP(" scm-var ");\n")
                       '(scm-var "= (" c-var ") ? SCM_BOOL_T : SCM_BOOL_F;\n"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:char>
  (gw:wrap-simple-type ws '<gw:char> "char"
                       '("SCM_NFALSEP(scm_char_p(" scm-var "))\n")
                       '(c-var "= SCM_CHAR(" scm-var ");\n")
                       '(scm-var "= SCM_MAKE_CHAR(" c-var ");\n"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:float>
  (gw:wrap-simple-type ws '<gw:float> "float"
                       '("SCM_NFALSEP(scm_number_p(" scm-var "))\n")
                       '(c-var "= scm_num2float(" scm-var ", 1,"
                               " \"gw:scm->float\");\n")
                       '(scm-var "= scm_float2num(" c-var ");\n"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:double>
  (gw:wrap-simple-type ws '<gw:double> "double"
                       '("SCM_NFALSEP(scm_number_p(" scm-var "))\n")
                       '(c-var "= scm_num2double(" scm-var ", 1,"
                               " \"gw:scm->double\");\n")
                       '(scm-var "= scm_double2num(" c-var ");\n"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:int>
  (let ((wt (wrap-simple-ranged-signed-integer-type
             ws '<gw:int> "int"
             "scm_long2num(INT_MIN)"
             "scm_long2num(INT_MAX)"
             '(c-var "= gh_scm2long(" scm-var ");\n")
             '(scm-var "= gh_long2scm(" c-var ");\n"))))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:unsigned-int>
  (let ((wt (wrap-simple-ranged-unsigned-integer-type
             ws '<gw:unsigned-int> "unsigned int"
             "scm_ulong2num(UINT_MAX)"
             '(c-var "= gh_scm2ulong(" scm-var ");\n")
             '(scm-var "= gh_ulong2scm(" c-var ");\n"))))
    (set! limits-requiring-types (cons wt limits-requiring-types)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:long>
  (let ((wt (wrap-simple-ranged-signed-integer-type
             ws '<gw:long> "long"
             "scm_long2num(LONG_MIN)"
             "scm_long2num(LONG_MAX)"
             '(c-var "= gh_scm2long(" scm-var ");\n")
             '(scm-var "= gh_long2scm(" c-var ");\n"))))
    (set! limits-requiring-types (cons wt limits-requiring-types)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <gw:unsigned-long>
  (let ((wt (wrap-simple-ranged-unsigned-integer-type
             ws '<gw:unsigned-long> "unsigned long"
             "scm_ulong2num(ULONG_MAX)"
             '(c-var "= gh_scm2ulong(" scm-var ");\n")
             '(scm-var "= gh_ulong2scm(" c-var ");\n"))))
    (set! limits-requiring-types (cons wt limits-requiring-types)))

  ;; long long support is currently unavailable.  To fix that, we're
  ;; going to need to do some work to handle broken versions of guile
  ;; (or perhaps just refuse to add long long support for those
  ;; versions.  The issue is that some versions of guile in
  ;; libguile/__scm.h just "typedef long long_long" even on platforms
  ;; that have long long's that are larger than long.  This is a mess,
  ;; meaning, among other things, that long_long won't be big enough
  ;; to hold LONG_LONG_MAX, etc.  yuck.  (NOTE: <gw:gint64 should now
  ;; work -- use that as a substitute if you can...)

  (let* ((mchars (gw:wrap-type ws '<gw:mchars>)))
    
    (define (c-type-name-func typespec)
      (if (memq 'const (gw:typespec-get-options typespec))
          "const char *"
          "char *"))

    (define (typespec-options-parser options-form wrapset)
      (let ((remainder options-form))
        (set! remainder (delq 'const remainder))
        (if (and (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder))
            (throw 'gw:bad-typespec
                   "Bad <gw:mchars> options form (caller and callee owned!)."
                   options-form))
        (if (not (or (memq 'caller-owned remainder)
                     (memq 'callee-owned remainder)))
            (throw 'gw:bad-typespec
                   "Bad <gw:mchars> options form (must be caller or callee owned!)."
                   options-form))
        (set! remainder (delq 'caller-owned remainder))
        (set! remainder (delq 'callee-owned remainder))
        (if (null? remainder)
            options-form
            (throw 'gw:bad-typespec
                   "Bad <gw:mchars> options form - spurious options: "
                   remainder))))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (list
       c-var " = NULL;\n"
       "\n"
       "if(SCM_FALSEP(" scm-var "))\n"
       "  " c-var " = NULL;\n"
       "else if(SCM_STRINGP(" scm-var "))\n"
       "  " c-var " = gh_scm2newstr(" scm-var ", NULL);\n"
       "else\n"
       `(gw:error ,status-var type ,scm-var)))
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (list
       "  /* we coerce to (char *) here b/c broken guile 1.3.4 prototype */\n"
       "if(" c-var " == NULL) " scm-var " = SCM_BOOL_F;\n"
       "else "
       scm-var " = gh_str02scm((char *) " c-var ");\n"))
    
    (define (c-destructor c-var typespec status-var force?)
      (if (or force?
              (memq 'caller-owned (gw:typespec-get-options typespec)))
          (list "if(" c-var ") free((void *) " c-var ");\n")
          '()))
    
    (define (pre-call-arg-ccg param status-var)
      (let* ((scm-name (gw:param-get-scm-name param))
             (c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (list
         (scm->c-ccg c-name scm-name typespec status-var)
         "if(" `(gw:error? ,status-var type) ")"
         `(gw:error ,status-var arg-type)
         "else if(" `(gw:error? ,status-var range) ")"
         `(gw:error ,status-var arg-range))))
    
    (define (call-ccg result func-call-code status-var)
      (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
    
    (define (post-call-arg-ccg param status-var)
      (let* ((c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (c-destructor c-name typespec status-var #f)))
    
    (define (post-call-result-ccg result status-var)
      (let* ((scm-name (gw:result-get-scm-name result))
             (c-name (gw:result-get-c-name result))
             (typespec (gw:result-get-typespec result)))
        (list
         (c->scm-ccg scm-name c-name typespec status-var)
         (c-destructor c-name typespec status-var #f))))

    (gw:type-set-c-type-name-func! mchars c-type-name-func)
    (gw:type-set-typespec-options-parser! mchars typespec-options-parser)
    
    (gw:type-set-scm->c-ccg! mchars scm->c-ccg)
    (gw:type-set-c->scm-ccg! mchars c->scm-ccg)
    (gw:type-set-c-destructor! mchars c-destructor)  
    
    (gw:type-set-pre-call-arg-ccg! mchars pre-call-arg-ccg)
    (gw:type-set-call-ccg! mchars call-ccg)
    (gw:type-set-post-call-arg-ccg! mchars post-call-arg-ccg)
    (gw:type-set-post-call-result-ccg! mchars post-call-result-ccg)
    
    mchars)

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
     (if (and client-wrapset
              (gw:any? (lambda (x) (gw:wrapset-uses-type? client-wrapset x))
                       limits-requiring-types))
         "#include <limits.h>\n"
         '())))

  )
