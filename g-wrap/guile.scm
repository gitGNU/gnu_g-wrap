(define-module (g-wrap guile)
  #:use-module (srfi srfi-1)
  
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap enumeration)
  #:use-module (g-wrap c-types)

  #:duplicates last
  
  #:export (inline-scheme
            
            <gw-guile> guile
                       
            <gw-guile-wrapset>
            module module-exports
            add-module-export!
            
            <gw-guile-simple-type>
            scm-var))




;; Utility stuff
(define (guile-module-name->c-registration-strlist name-symlist)
  (separate-by (map symbol->string name-symlist) " "))

(define (guile-module-name->c-sym-name-strlist name-symlist)
  (separate-by
   (map (lambda (s) (gw:any-str->c-sym-str (symbol->string s)))
        name-symlist)
   "_"))

(define-method (scm-var (value <gw-value>))
  (string-append "*(SCM *)" (wrapped-var value)))

(define-class <gw-guile> (<gw-language>))

(define guile (make <gw-guile>))



;;;
;;; Wrapset
;;;

(define-class <gw-guile-wrapset> (<gw-rti-wrapset>)
  (module #:init-keyword #:module #:accessor module)
  (module-exports #:getter module-exports #:init-value '()))

(define-method (initialize (wrapset <gw-guile-wrapset>) initargs)
  (next-method)
    
  (let ((wrapset-name-c-sym (any-str->c-sym-str
                             (symbol->string (name wrapset)))))
    
    (define (init-wrapper-cg lang)
      (list "void gw_guile_init_wrapset_" wrapset-name-c-sym "(void);\n"
            "void gw_guile_init_wrapset_" wrapset-name-c-sym "(void)\n"
            "{\n"
            "  gw_init_wrapset_" wrapset-name-c-sym "(NULL);\n"
            "}\n"))
    
    ;; FIXME: this should be an initializer; we use a declarator to have
    ;; this as first call...
    (add-cs-declarator! wrapset (lambda (lang)
                                  (list "gw_guile_runtime_init ();\n")))
    (add-cs-global-declarator!
     wrapset (lambda (lang)
               (list "#include <g-wrap/guile-runtime.h>\n")))
    (add-cs-definer! wrapset init-wrapper-cg)))
  
;;; Additional methods

(define-method (add-module-export! (ws <gw-guile-wrapset>) (sym <symbol>))
  (slot-set! ws 'module-exports (cons sym (slot-ref ws 'module-exports))))

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

(define-method (add-constant! (wrapset <gw-guile-wrapset>)
                              (constant <gw-constant>))

  (define (cs-initializer lang error-var)
    (let* ((scm-var (gen-c-tmp "scm_wrapped_value"))
           (wrap-value-code
            (wrap-value-cg lang (type constant)
                           (make <gw-value>
                             #:var (value constant)
                             #:wrapped-var (string-append "&" scm-var))
                           error-var)))
      (list
       "{\n"
        "  SCM " scm-var ";\n"
        "\n"
        wrap-value-code
        "if (!" `(gw:error? ,error-var) ")"
        "  scm_c_define (\"" (symbol->string (name constant)) "\", " scm-var ");\n"
        "}\n")))
  
  (next-method)
  
  (add-module-export! wrapset (name constant))
  (add-cs-initializer! wrapset cs-initializer))

(define-method (add-function! (wrapset <gw-guile-wrapset>)
                              (func <gw-function>))
  
  (next-method)
  
  (add-module-export! wrapset (name func)))


;;;
;;; RTI
;;;

;; TODO: We don' get full error messages yet (#f passed as param to
;; expand-special-forms)

(define-method (wrap-value-function-cg (lang <gw-guile>)
                                       (type <gw-rti-type>))
  (let* ((type-name (c-type-name type))
         (value (make <gw-rti-value>
                  #:var (string-append "(*(" type-name "*)instance)")
                  #:typespec "*typespec"
                  #:wrapped-var "value")))
    (list
     "static void " (wrap-value-function-name type)
     "(GWLangLocative value, GWLangArena arena, const GWTypeSpec *typespec, void *instance, GWError *error) {\n"
     "  " (expand-special-forms (wrap-value-cg lang type value "*error")
                                #f '(type arg-type range memory misc))
     "}\n")))


(define-method (unwrap-value-function-cg (lang <gw-guile>)
                                         (type <gw-rti-type>))
  (let* ((type-name (c-type-name type))
         (value (make <gw-rti-value>
                  #:var (string-append "(*(" type-name "*)instance)")
                  #:typespec "*typespec"
                  #:wrapped-var "value")))
    (list
     "static void " (unwrap-value-function-name type)
     "(void *instance, GWLangArena arena, const GWTypeSpec *typespec, GWLangLocative value, GWError *error) {\n"
     "  " (expand-special-forms (unwrap-value-cg lang type value "*error")
                                #f '(type arg-type range memory misc))
     "}\n")))

(define-method (destruct-value-function-cg (lang <gw-guile>)
                                           (type <gw-rti-type>))
  
  (let* ((type-name (c-type-name type))
         (value (make <gw-rti-value>
                  #:var (string-append "(*(" type-name "*)instance)")
                  #:typespec "*typespec"
                  #:wrapped-var "value")))
    (list
     "static void " (destruct-value-function-name type)
     "(GWLangArena arena, void *instance, const GWTypeSpec *typespec, GWError *error) {\n"
     "  " (expand-special-forms
           (destruct-value-cg lang type value "*error")
           #f '(type arg-type range memory misc))
     "}\n")))

;;;
;;; Enumerations
;;;

(define-class <gw-guile-enum> (<gw-enumeration-type>)
  val->int-c-func
  val->int-scm-func
  val->sym-c-func
  val->sym-scm-func)

(define-method (initialize (enum <gw-guile-enum>) initargs)
  (define (gen-name action) (gen-c-tmp-name enum action)) ;; Just lazy
  (next-method)

  (slot-set! enum 'val->int-c-func (gen-name "val_to_int"))
  (slot-set! enum 'val->int-scm-func
             (string-append "enum-" (symbol->string (name enum)) "-val->int"))
  
  (slot-set! enum 'val->sym-c-func (gen-name "val_to_sym"))
  (slot-set! enum 'val->sym-scm-func 
             (string-append "enum-" (symbol->string (name enum)) "-val->sym")))

(define-method (global-definitions-cg (lang <gw-guile>)
                                      (wrapset <gw-guile-wrapset>)
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

(define-method (initializations-cg (lang <gw-guile>)
                                   (wrapset <gw-guile-wrapset>)
                                   (enum <gw-enumeration-type>)
                                   error-var)
  (list
   "scm_c_define_gsubr (\"" (slot-ref enum 'val->int-scm-func) "\", 1, 0, 0,\n"
   "                      " (slot-ref enum 'val->int-c-func) ");\n"
   "scm_c_define_gsubr (\"" (slot-ref enum 'val->sym-scm-func) "\", 2, 0, 0,\n"
   "                      " (slot-ref enum 'val->sym-c-func) ");\n"))

(define-method (wrap-value-cg (lang <gw-guile>)
                              (type <gw-guile-enum>)
                              (value <gw-value>)
                              status-var)
  (list (scm-var value) " = scm_long2num(" (var value) ");\n"))

(define-method (unwrap-value-cg (lang <gw-guile>)
                                (enum <gw-guile-enum>)
                                (value <gw-value>)
                                status-var)
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

(define-class <gw-guile-simple-type> (<gw-simple-rti-type>)
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

(define-method (unwrap-value-cg (lang <gw-language>)
                                (type <gw-guile-simple-type>)
                                (value <gw-value>)
                                status-var)
  (let* ((scm-var (scm-var value))
         (c-var (var value))
         (unwrap-code (replace-syms (slot-ref type 'unwrap)
                                    `((c-var . ,c-var)
                                      (scm-var . ,scm-var))))
         (type-check (slot-ref type 'type-check)))
    (if type-check
        (list "if (!(" (replace-syms type-check `((scm-var . ,scm-var))) "))"
              `(gw:error ,status-var type ,scm-var)
              "else {" unwrap-code "}")
        unwrap-code)))

(define-method (wrap-value-cg (lang <gw-language>)
                              (type <gw-guile-simple-type>)
                              (value <gw-value>)
                              status-var)
  (replace-syms (slot-ref type 'wrap)
                `((c-var . ,(var value))
                  (scm-var . ,(scm-var value)))))

(define-method (wrap-simple-type! (wrapset <gw-guile-wrapset>) . args)
  (let ((type (apply make <gw-guile-simple-type> args)))
    (add-type! wrapset type)))
                                  


;;;
;;; Wrapped C Types
;;;

(define-class <gw-guile-wct> (<gw-wct>)
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

(define-method (wrap-value-cg (lang <gw-guile>)
                              (wct <gw-guile-wct>)
                              (value <gw-value>)
                              status-var)
  (let ((wct-var (slot-ref wct 'wct-var-name))
        (sv (scm-var value))
        (cv (var value)))
    (list
     "if(" cv " == NULL) " sv " = SCM_BOOL_F;\n"
     "else " sv " = gw_wcp_assimilate_ptr((void *) " cv ", " wct-var ");\n")))


(define-method (unwrap-value-cg (lang <gw-guile>)
                                (wct <gw-guile-wct>)
                                (value <gw-value>)
                                status-var)
  (let ((wct-var (slot-ref wct 'wct-var-name))
        (sv (scm-var value))
        (c-var (var value)))
    (list
     "if (SCM_FALSEP(" sv "))\n"
     "  " c-var " = NULL;\n"
     "else if (gw_wcp_is_of_type_p(" wct-var ", " sv "))\n"
     "  " c-var " = gw_wcp_get_ptr(" sv ");\n"
     "else\n"
     `(gw:error ,status-var type ,sv))))

(define-method (initializations-cg (lang <gw-guile>)
                                   (wrapset <gw-wrapset>)
                                   (wct <gw-guile-wct>)
                                   error-var)
  (let ((wct-var (slot-ref wct 'wct-var-name))
        (wcp-type-name (symbol->string (name wct))))
  (list
   wct-var "= gw_wct_create(\"" wcp-type-name "\", NULL, NULL, NULL, NULL);\n"
   "scm_c_define(\"" wcp-type-name "\", " wct-var ");\n")))

(define-method (global-declarations-cg (lang <gw-guile>)
                                       (wrapset <gw-wrapset>)
                                       (wct <gw-guile-wct>))
  (list "SCM "  (slot-ref wct 'wct-var-name) " = SCM_BOOL_F;\n"))


;;;
;;; Generation
;;;

(define-method (generate-wrapset (lang <gw-guile>)
                                 (wrapset <gw-guile-wrapset>)
                                 (basename <string>))
  (next-method)

  (if (module wrapset)
      (let* ((wrapset-name (name wrapset))
             (wrapset-name-c-sym (any-str->c-sym-str
                                  (symbol->string wrapset-name)))
             (wrapset-scm-file-name (string-append basename ".scm"))
             (guile-module (module wrapset))
             (guile-module-exports (module-exports wrapset)))
    
    (call-with-output-file wrapset-scm-file-name
      (lambda (port)
        
        (define (dsp-list lst)
          (for-each (lambda (s) (display s port)) lst))
        
        (flatten-display
         (list
          ";; Generated by G-Wrap-TNG: an experimental Guile C API-wrapper engine.\n"
          "\n"
          (format #f "(define-module ~S\n" guile-module)
          (fold (lambda (ws clauses)
                  (if (module ws)
                      (cons (format #f "  #:use-module ~S\n" (module ws))
                            clauses)
                      clauses))
                '()
                (wrapsets-depended-on wrapset))
          "  #:export (" (map
                          (lambda (sym)
                            (list sym " "))
                          (module-exports wrapset))
          "))\n"
          "\n"
          "(dynamic-call \"gw_guile_init_wrapset_" wrapset-name-c-sym "\"\n"
          "              (dynamic-link \"libgw-guile-" wrapset-name "\"))\n")
         port))))))

      
