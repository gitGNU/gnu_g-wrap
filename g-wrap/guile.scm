(define-module (g-wrap guile)
  #:use-module (srfi srfi-1)
  
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  #:use-module (g-wrap ffi)
  #:use-module (g-wrap enumeration)

  #:duplicates last
  
  #:export (<gw-guile> guile
            <gw-guile-wrapset>
            <gw-guile-simple-type>))




;; Utility stuff
(define (guile-module-name->c-registration-strlist name-symlist)
  (separate-by (map symbol->string name-symlist) " "))

(define (guile-module-name->c-sym-name-strlist name-symlist)
  (separate-by
   (map (lambda (s) (gw:any-str->c-sym-str (symbol->string s)))
        name-symlist)
   "_"))

(define-class <gw-guile> (<gw-language>))

(define guile (make <gw-guile>))

(define-class <gw-guile-wrapset> (<gw-ffi-wrapset>)
  (module #:init-keyword #:module #:getter module)
  (module-exports #:getter module-exports #:init-value '()))

(define-method (initialize (wrapset <gw-guile-wrapset>) initargs)
  (next-method)
  
  ;; FIXME: this should be an initializer; we use a declarator to have
  ;; this as first call...
  (add-cs-declarator! wrapset (lambda (lang)
                                (list "gw_guile_runtime_init ();\n")))
  (add-cs-global-declarator!
   wrapset (lambda (lang)
             (list "#include <g-wrap/guile-runtime.h>\n"))))
  

(define-method (add-module-export! (ws <gw-guile-wrapset>) (sym <symbol>))
  (slot-set! ws 'module-exports (cons sym (slot-ref ws 'module-exports))))

;; TODO: We don' get full error messages yet (#f passed as param to
;; expand-special-forms)

(define-method (wrap-value-function-cg (lang <gw-guile>)
                                       (type <gw-ffi-type>))
  (let* ((type-name (c-type-name type))
         (value (make <gw-value>
                  #:var (string-append "(*(" type-name "*)instance)")
                  #:typespec "*typespec"
                  #:wrapped-var "value")))
    (list
     "static void " (wrap-value-function-name type)
     "(void *instance, const GWTypeSpec *typespec, GWLangValue *value, GWError *error) {\n"
     "  " (expand-special-forms (wrap-value-cg lang type value "*error")
                                #f '(type arg-type range memory misc))
     "}\n")))


(define-method (unwrap-value-function-cg (lang <gw-guile>)
                                         (type <gw-ffi-type>))
  (let* ((type-name (c-type-name type))
         (value (make <gw-value>
                  #:var (string-append "(*(" type-name "*)instance)")
                  #:typespec "*typespec"
                  #:wrapped-var "value")))
    (list
     "static void " (unwrap-value-function-name type)
     "(void *instance, const GWTypeSpec *typespec, GWLangValue *value, GWError *error) {\n"
     "  " (expand-special-forms (unwrap-value-cg lang type value "*error")
                                #f '(type arg-type range memory misc))
     "}\n")))

(define-method (destruct-value-function-cg (lang <gw-guile>)
                                           (type <gw-type>))
  
  (let* ((type-name (c-type-name type))
         (value (make <gw-value>
                  #:var (string-append "(*(" type-name "*)instance)")
                  #:typespec "*typespec"
                  #:wrapped-var "value")))
    (list
     "static void " (destruct-value-function-name type)
     "(void *instance, const GWTypeSpec *typespec, GWError *error) {\n"
     "  " (expand-special-forms
           (destruct-value-cg lang type value "*error")
           #f '(type arg-type range memory misc))
     "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enumerations
;;

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
   "scm_c_define_gsubr (\"" (slot-ref enum 'val->sym-scm-func) "\", 1, 0, 0,\n"
   "                      " (slot-ref enum 'val->sym-c-func) ");\n"))

(define-method (wrap-value-cg (lang <gw-guile>)
                              (type <gw-guile-enum>)
                              (value <gw-value>)
                              status-var)
  (list (wrapped-var value) " = scm_long2num(" (var value) ");\n"))

(define-method (unwrap-value-cg (lang <gw-guile>)
                                (enum <gw-guile-enum>)
                                (value <gw-value>)
                                status-var)
  (let ((scm-var (wrapped-var value))
        (c-var (var value))
        (val-sym-array-name (val-array-name enum)))
    (list
     scm-var " = gw_guile_enum_val2int(" val-sym-array-name ", " scm-var ");\n"
     "if(SCM_FALSEP(scm_integer_p(" scm-var ")))"
     `(gw:error ,status-var type ,scm-var)
     "else " c-var " = scm_num2long(" scm-var
     ", 0, \"%gw:enum->scm->c-ccg\");\n")))
  

(define-method (wrap-enum! (wrapset <gw-guile-wrapset>) . args)
  (add-type! wrapset (apply make <gw-guile-enum> args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <gw-guile-simple-type>
;;
(define-class <simple-type> (<gw-ffi-type>)
  (type-check #:init-keyword #:type-check)
  (wrap #:init-keyword #:wrap)
  (unwrap #:init-keyword #:unwrap))

(define <gw-guile-simple-type> <simple-type>)

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
                                (type <simple-type>)
                                (value <gw-value>)
                                status-var)
  (let* ((scm-var (string-append "*((SCM *)" (wrapped-var value) ")"))
         (c-var (var value))
         (type-check-code (replace-syms (slot-ref type 'type-check)
                                        `((scm-var . ,scm-var))))
         (unwrap-code (replace-syms (slot-ref type 'unwrap)
                                    `((c-var . ,c-var)
                                      (scm-var . ,scm-var)))))
    (list "if (!(" type-check-code "))"
          `(gw:error ,status-var type ,scm-var)
          "else {" unwrap-code "}")))

(define-method (wrap-value-cg (lang <gw-language>)
                              (type <simple-type>)
                              (value <gw-value>)
                              status-var)
  (replace-syms (slot-ref type 'unwrap)
                `((c-var . ,(var value))
                  (scm-var . ,(string-append
                               "*((SCM *)" (wrapped-var value) ")")))))

(define-method (add-constant! (wrapset <gw-guile-wrapset>)
                              (constant <gw-constant>))

  (define (cs-initializer lang error-var)
    (let* ((scm-var (gen-c-tmp "scm_wrapped_value"))
           (wrap-value-code
            (wrap-value-cg lang (type constant)
                           (make <gw-value>
                             #:var (value constant)
                              #:wrapped-var scm-var)
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
  
;   (define (cs-initializer lang error-var)
;     (let* ((scm-var (gen-c-tmp "scm_wrapped_value"))
;            (wrap-value-code
;             (wrap-value-cg lang (type constant)
;                            (make <gw-value>
;                              #:var (value constant)
;                              #:wrapped-var scm-var)
;                            error-var)))
;       (list
;        "{\n"
;        "  SCM " scm-var ";\n"
;        "\n"
;        wrap-value-code
;        "if (!" `(gw:error? ,error-var) ")"
;         "  scm_c_define (\"" (symbol->string (name constant)) "\", " scm-var ");\n"
;         "}\n")))
  
  (next-method)
  
  (add-module-export! wrapset (name func))
;;  (add-cs-initializer! wrapset cs-initializer))
  )

(define-method (generate-wrapset (lang <gw-guile>)
                                 (wrapset <gw-guile-wrapset>)
                                 (basename <string>))
  (next-method)

  (if (module wrapset)
      (let* ((wrapset-name (name wrapset))
             (wrapset-name-c-sym (any-str->c-sym-str wrapset-name))
             (wrapset-scm-file-name (string-append wrapset-name ".scm"))
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
          "(dynamic-call \"gw_init_wrapset_" wrapset-name-c-sym "\"\n"
          "              (dynamic-link \"lib" wrapset-name "\"))\n")
         port))))))

      
