;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enumeration wrapper type.
;;;

;;; TODO
;;;
;;;   switch to using gw:tmp-c-sym

(define-module (g-wrap enumeration)
  :use-module (g-wrap)
  :use-module (g-wrap output-file))

(define-public (gw:wrap-enumeration wrapset name-sym c-type-name)

  (let* ((wt (gw:wrap-type wrapset name-sym))
         (enum-c-sym
          (gw:any-str->c-sym-str (symbol->string (gw:type-get-name wt))))
         (val->int-var-name
          (string-append "gw__enum_" enum-c-sym "_val_to_int_scm_func"))
         (val->int-scm-func-name
          (let* ((enum-name (hashq-ref wt 'gw:name #f)))
            (string-append "gw:enum-" (symbol->string enum-name) "-val->int")))
         (val->int-c-func-name
          (string-append "gw__enum_" enum-c-sym "_val_to_int"))
         (val->sym-var-name
          (string-append "gw__enum_" enum-c-sym "_val_to_sym_scm_func"))
         (val->sym-scm-func-name
          (let* ((enum-name (hashq-ref wt 'gw:name #f)))
            (string-append "gw:enum-" (symbol->string enum-name) "-val->sym")))
         (val->sym-c-func-name
          (string-append "gw__enum_" enum-c-sym "_val_to_sym")))
    
    (define (c-type-name-func typespec)
      (if (memq 'const (gw:typespec-get-options typespec))
          (string-append "const " c-type-name)
          c-type-name))
    
    (define (typespec-options-parser options-form wrapset)
      (let ((remainder options-form))
        (set! remainder (delq 'const options-form))
        (if (null? remainder)
            options-form
            (throw 'gw:bad-typespec "Bad enumeration options form."
                   options-form))))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (list
       scm-var " = gh_call1(" val->int-var-name ", " scm-var ");\n"
       "if(SCM_FALSEP(scm_integer_p(" scm-var ")))"
       `(gw:error ,status-var type ,scm-var)
       "else " c-var " = gh_scm2long(" scm-var ");\n"))
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (list scm-var " = gh_long2scm(" c-var ");\n"))
    
    (define (global-declarations-ccg type client-wrapset)
      (if (eq? client-wrapset wrapset)
          '()
          (list "static SCM " val->int-var-name ";\n"
                "static SCM " val->sym-var-name ";\n")))
    
    (define (global-definitions-ccg type client-wrapset)
      (if client-wrapset
          '()
          (let* ((substitutions
                  `((enum-c-type-name ,c-type-name)
                    (enum-c-sym ,enum-c-sym)
                    (val-to-int-func ,val->int-var-name)
                    (val-to-sym-func ,val->sym-var-name)
                    (val->int-c-func-name ,val->int-c-func-name)
                    (val->sym-c-func-name ,val->sym-c-func-name)
                    (val->sym-logic
                     ,(map
                       (lambda (enum-val)
                         (let ((c-sym (car enum-val))
                               (scm-sym (cdr enum-val)))
                           (list
                            "\n"
                            "  if(gw__enum_val == " c-sym ") {\n"
                            "    if(!gw__return_all_syms) return gh_symbol2scm(\"" scm-sym "\");\n"
                            "    gw__scm_result =\n"
                            "      gh_cons(gh_symbol2scm(\"" scm-sym "\"), gw__scm_result);\n"
                            "  }\n")))
                       (hashq-ref type 'enum:values)))
                    (val->int-logic
                     ,(map
                       (lambda (enum-val)
                         (let ((c-sym (car enum-val))
                               (scm-sym (cdr enum-val)))
                           (list
                            "\n"
                            "  if(strcmp(gw__symstr, \"" scm-sym "\") == 0) {\n"
                            "    free(gw__symstr);\n"
                            "    return gh_long2scm(" c-sym ");\n"
                            "  }\n")))
                       (hashq-ref type 'enum:values))))))
            
            (translate-str "\
static SCM
%val->sym-c-func-name%(SCM gw__scm_val, SCM gw__scm_show_all_p) {
  %enum-c-type-name% gw__enum_val;
  SCM gw__scm_result;

  int gw__return_all_syms = SCM_NFALSEP(gw__scm_show_all_p);

  if(gw__return_all_syms) gw__scm_result = SCM_EOL;
  else gw__scm_result = SCM_BOOL_F;

  if(gh_symbol_p(gw__scm_val)) {
    SCM gw__scm_int_value = gh_call1(%val-to-int-func%,
                                     gw__scm_val);
    if(SCM_FALSEP(gw__scm_int_value)) return SCM_EOL;
    if(!gw__return_all_syms) return gw__scm_val;
    gw__enum_val = gh_scm2long(gw__scm_int_value);
  } else {
    /* this better be an int */
    gw__enum_val = gh_scm2long(gw__scm_val);
  }

  %val->sym-logic%
  
  return(gw__scm_result);
}

static SCM
%val->int-c-func-name%(SCM gw__scm_val) {
  char *gw__symstr = NULL;

  if(SCM_NFALSEP(scm_integer_p(gw__scm_val))) {
    SCM gw__scm_existing_sym = gh_call2(%val-to-sym-func%,
                                        gw__scm_val,
                                        SCM_BOOL_F);
    if(SCM_FALSEP(gw__scm_existing_sym)) {
      return SCM_BOOL_F;
    } else {
      return gw__scm_val;
    }
  }

  gw__symstr = gh_symbol2newstr(gw__scm_val, NULL);

  %val->int-logic%

  free(gw__symstr);
  return SCM_BOOL_F;
}
"
                           substitutions))))

    ;; TODO: maybe set status-var on failure...
    (define (global-init-ccg type client-wrapset status-var)
      (cond
       ;; self-client.
       ((eq? client-wrapset wrapset)
        '())
       ;; regular client
       (client-wrapset
        (list
         val->int-var-name " = SCM_VARIABLE_REF (scm_c_lookup(\"" val->int-scm-func-name "\");\n"
         val->sym-var-name " = SCM_VARIABLE_REF (scm_c_lookup(\"" val->sym-scm-func-name "\");\n"))
       ;; normal wrapset type code.
       (else
        (list
         "\n"
         "    " val->int-var-name " =\n"
         "      scm_c_define_gsubr (\"" val->int-scm-func-name "\", 1, 0, 0,\n"
         "                         " val->int-c-func-name ");\n"
         "    " val->sym-var-name " =\n"
         "      scm_c_define_gsubr (\"" val->sym-scm-func-name "\", 2, 0, 0,\n"
         "                         " val->sym-c-func-name ");\n"))))
    
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
    
    (hashq-set! wt 'enum:values '())
    
    (gw:type-set-c-type-name-func! wt c-type-name-func)
    (gw:type-set-typespec-options-parser! wt typespec-options-parser)
    
    (gw:type-set-scm->c-ccg! wt scm->c-ccg)
    (gw:type-set-c->scm-ccg! wt c->scm-ccg)
    
    (gw:type-set-global-declarations-ccg! wt global-declarations-ccg)
    (gw:type-set-global-definitions-ccg! wt global-definitions-ccg)
    (gw:type-set-global-initializations-ccg! wt global-init-ccg)
    
    (gw:type-set-pre-call-arg-ccg! wt pre-call-arg-ccg)
    (gw:type-set-call-ccg! wt call-ccg)
    (gw:type-set-post-call-result-ccg! wt post-call-result-ccg)
    
    (gw:wrapset-add-guile-module-export! wrapset
                                         (string->symbol val->int-scm-func-name))
    (gw:wrapset-add-guile-module-export! wrapset
                                         (string->symbol val->sym-scm-func-name))
    
    wt))

(define-public (gw:enum-add-value! enum c-val-namestr scheme-sym)
  ;; FIXME: need checking for duplicate values here...
  (hashq-set! enum 'enum:values
              (cons (cons c-val-namestr scheme-sym)
                    (hashq-ref enum 'enum:values))))
