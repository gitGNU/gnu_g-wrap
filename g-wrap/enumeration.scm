;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enumeration wrapper type.
;;;

;;; TODO
;;;
;;;   switch to using gw:tmp-c-sym

(define-module (g-wrap enumeration)
  #:use-module (g-wrap)
  #:use-module (g-wrap dynamic-type)
  #:use-module (g-wrap output-file))

(define-public (gw:wrap-enumeration wrapset name-sym c-type-name)

  (let* ((enum-c-sym
          (gw:any-str->c-sym-str (symbol->string name-sym)))
         (val-sym-array-name
          (string-append "gw__enum_" enum-c-sym "_val_array"))
         (val->int-var-name 
          (string-append "gw__enum_" enum-c-sym "_val_to_int_scm_func"))
         (val->int-scm-func-name
            (string-append "gw:enum-" (symbol->string name-sym) "-val->int"))
         (val->int-c-func-name
          (string-append "gw__enum_" enum-c-sym "_val_to_int"))
         (val->sym-var-name
          (string-append "gw__enum_" enum-c-sym "_val_to_sym_scm_func"))
         (val->sym-scm-func-name
          (string-append "gw:enum-" (symbol->string name-sym) "-val->sym"))
         (val->sym-c-func-name
          (string-append "gw__enum_" enum-c-sym "_val_to_sym")))
    
    (define (typespec-options-parser options-form wrapset)
      (let ((remainder options-form))
        (set! remainder (delq 'const options-form))
        (if (null? remainder)
            (cons 'caller-owned options-form) ; needed for dynamic typespec
            (throw 'gw:bad-typespec "Bad enumeration options form."
                   options-form))))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (list
       scm-var " = gw_enum_val2int(" val-sym-array-name ", " scm-var ");\n"
       "if(SCM_FALSEP(scm_integer_p(" scm-var ")))"
       `(gw:error ,status-var type ,scm-var)
       "else " c-var " = scm_num2long(" scm-var
       ", 0, \"%gw:enum->scm->c-ccg\");\n"))
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (list scm-var " = scm_long2num(" c-var ");\n"))

    (define (c-destructor c-var typespec status-var force?)
      '())
    
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
                    (val-sym-array-name ,val-sym-array-name)
                    (val-to-int-func ,val->int-var-name)
                    (val-to-sym-func ,val->sym-var-name)
                    (val->int-c-func-name ,val->int-c-func-name)
                    (val->sym-c-func-name ,val->sym-c-func-name)
                    (val-sym-array 
                     ,(list
                      "{\n"
                      (map
                       (lambda (enum-val)
                         (let ((c-sym (car enum-val))
                               (scm-sym (cdr enum-val)))
                           (list
                            "  {" c-sym ", \"" scm-sym "\" },\n")))
                       (hashq-ref type 'enum:values))
                      " { 0, NULL } }")))))
            
            (translate-str "\

static GWEnumPair %val-sym-array-name%[] = %val-sym-array%;

static SCM
%val->sym-c-func-name%(SCM gw__scm_val, SCM gw__scm_show_all_p) {
  return gw_enum_val2sym(%val-sym-array-name%, gw__scm_val, 
                         gw__scm_show_all_p);
}

static SCM
%val->int-c-func-name%(SCM gw__scm_val) {
  return gw_enum_val2int(%val-sym-array-name%, gw__scm_val);
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
    
    (let* ((wt (gw:wrap-dynamic-type
                wrapset name-sym
                c-type-name (string-append "const " c-type-name)
                scm->c-ccg
                c->scm-ccg
                c-destructor
                'uint))) ;; FIMXE: are enumns are passed as ints always?
    
      (hashq-set! wt 'enum:values '())
      
      (gw:type-set-typespec-options-parser! wt typespec-options-parser)
    
      (gw:type-set-global-declarations-ccg! wt global-declarations-ccg)
      (gw:type-set-global-definitions-ccg! wt global-definitions-ccg)
      (gw:type-set-global-initializations-ccg! wt global-init-ccg)
      
      (gw:wrapset-add-guile-module-export! wrapset
                                           (string->symbol val->int-scm-func-name))
      (gw:wrapset-add-guile-module-export! wrapset
                                           (string->symbol val->sym-scm-func-name))
      
      wt)))

(define-public (gw:enum-add-value! enum c-val-namestr scheme-sym)
  ;; FIXME: need checking for duplicate values here...
  (hashq-set! enum 'enum:values
              (cons (cons c-val-namestr scheme-sym)
                    (hashq-ref enum 'enum:values))))
