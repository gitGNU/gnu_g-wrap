;; -*-scheme-*-

(define-module (g-wrap guile wct-spec)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap ffi)
  #:use-module (g-wrap guile standard-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wrapped C type (wct)
;;;
;;; A g-wrap wct can be used to wrap C types that don't have a natural
;;; representation on the Scheme side.  For a wct, g-wrap will
;;; generate a scheme side "proxy" that contains the C side data
;;; pointer.  Since issues of ownership (wrt deallocation) are
;;; critical, g-wrap helps handle that automatically.
;;;
;;; The wraptime fields of a non-native type are:
;;;   name -- <symbol> g-wrap type name
;;;   c-type-name -- <string>
;;;
;;;
;;; (**disabled for now**)
;;;   scm-rep-type-test-ccg -- thunk producing ccode to check that this arg is *really* OK (happens after WCP type check).
;;;
;;;   global-ccg
;;;   init-ccg
;;;   print-ccode -- thunk producing ccode to override default printer.
;;;   equal?-ccode -- thunk producing ccode to compare C side reps.
;;;   gc-mark-ccode -- code to "mark" this object's SCM contents.
;;;   cleanup-c-rep-ccode -- thunk producing ccode to "clean up" the C side rep.
;;;
;;; print -- (type result-var wcp-var port-var writing?-var)
;;; equal -- (type result-var wcp-a-var wcp-b-var)
;;; gc-mark -- (type result-var wcp-var)
;;; cleanup -- (type result-var wcp-a-var)

(define wrapsets-w-wct-initializers (make-hash-table 31))

(define-class <gw-wct> (<gw-ffi-type>)
  print-func-name
  equal?-func-name
  gc-mark-func-name)

(define-method (initialize (type <gw-wct>) initargs)
  (next-method)
  (slot-set! type 'print-func-name
             (string-append "gw__wct_print_for_"
                            (any-str->c-sym-str
                             (symbol->string (type-get-name type)))))
  (slot-set! type 'equal?-func-name
             (string-append "gw__wct_equal_p_for_"
                            (any-str->c-sym-str
                             (symbol->string (type-get-name type)))))
  (slot-set! type 'gc-mark-func-name
             (string-append "gw__wct_gc_mark_for_"
                            (any-str->c-sym-str
                             (symbol->string (type-get-name type)))))
  (slot-set! type 'cleanup-func-name
             (string-append "gw__wct_cleanup_for_"
                             (any-str->c-sym-str
                              (symbol->string (type-get-name type))))))

(define-generic wct-print-cg)
(define-generic wct-equal?-cg)
(define-generic wct-gc-mark-cg)
(define-generic wct-cleanup-cg)

(define-method (global-definitions-cg (lang <gw-guile>)
                                      (wrapset <gw-guile-wrapset>)
                                      (type <gw-wct>))
  
(define-public (wrap-as-wct wrapset name-sym c-type-name c-const-type-name)

  (let ((wct-var-name (gen-c-tmp
                       (string-append
                        "wct_info_for_"
                        (any-str->c-sym-str (symbol->string name-sym))))))
    
    (define (generate-print-func type func-name)
      (let ((func-ccg (hashq-ref type 'wct:print-ccg #f)))
        (list "int\n"
              func-name "(SCM gw__wcp, SCM gw__port, char gw__writing_p) {\n"
              "  int gw__result;\n"
              (func-ccg type
                        "gw__result"
                        "gw__wcp"
                        "gw__port"
                        "gw__writing_p")
              "}\n")))
    
    (define (generate-equal?-func type func-name)
      (let ((func-ccg (hashq-ref type 'wct:equal?-ccg #f)))
        (list "int\n"
              func-name "(SCM gw__wcp_a, SCM gw__wcp_b) {\n"
              "  int gw__result;\n"
              (func-ccg type "gw__result" "gw__wcp_a" "gw__wcp_b")
              "}\n")))
    
    (define (generate-gc-mark-func type func-name)
      (let ((func-ccg (hashq-ref type 'wct:gc-mark-ccg #f)))
        (list "SCM\n"
              func-name "(SCM gw__wcp) {\n"
              "  SCM gw__result;\n"
              (func-ccg type "gw__result" "gw__wcp")
              "}\n")))
    
    (define (generate-cleanup-func type func-name)
      (let ((func-ccg (hashq-ref type 'wct:gc-mark-ccg #f)))
        (list "scm_sizet\n"
              func-name "(SCM gw__wcp) {\n"
              "  scm_sizet gw__result;\n"
              (func-ccg type "gw__result" "gw__wcp")
              "}\n")))
    
    (define (typespec-options-parser options-form wrapset)
      (let ((remainder options-form))
        (set! remainder (delq 'const options-form))
        (if (null? remainder)
            (cons 'caller-owned options-form)
            (throw 'bad-typespec "Bad wct options form." options-form))))
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (let* ((sv scm-var)
             (wct-var wct-var-name)
             (type-check-code
              (list
               "SCM_FALSEP(" sv ") || gw_wcp_is_of_type_p(" wct-var ", " sv ")"))
             (scm->c-code
              (list
               "if(SCM_FALSEP(" sv ")) " c-var " = NULL;\n"
               "else " c-var " = gw_wcp_get_ptr(" sv ");\n")))
        
        (list "if(!(" type-check-code "))" `(error ,status-var type ,sv)
              "else {" scm->c-code "}\n")))
    
    (define (c->scm-ccg scm-var c-var typespec status-var)
      (let ((cv c-var)
            (sv scm-var)
            (wct-var wct-var-name))
        (list
         "if(" cv " == NULL) " sv " = SCM_BOOL_F;\n"
         "else " sv " = gw_wcp_assimilate_ptr((void *) " cv ", " wct-var ");\n")))

    (define (c-destructor c-var typespec status-var force?)
      '())
    
    (define (global-declarations-ccg type client-wrapset)    
      (if (eq? client-wrapset wrapset)
          '()
          (list "static SCM " wct-var-name " = SCM_BOOL_F;\n")))
    
    (define (global-definitions-ccg type client-wrapset)    
      (let* ((print-func-name (hashq-ref type 'wct:print-func-name #f))
             (equal?-func-name (hashq-ref type 'wct:equal?-func-name #f))
             (gc-mark-func-name (hashq-ref type 'wct:gc-mark-func-name #f))
             (cleanup-func-name (hashq-ref type 'wct:cleanup-func-name #f))
             (wct-global-ccg (hashq-ref type 'wct:global-ccg #f)))
        (if (not client-wrapset)
            (list
             (if print-func-name
                 (generate-print-func type print-func-name)
                 '())
             (if equal?-func-name
                 (generate-equal?-func type equal-func-name)
                 '())
             (if gc-mark-func-name
                 (generate-gc-mark-func type gc-mark-func-name)
                 '())
             (if cleanup-func-name
                 (generate-cleanup-func type cleanup-func-name)
                 '())
             (if wct-global-ccg
                 (wct-global-ccg type client-wrapset)
                 '()))
            '())))
    
    ;; TODO: maybe use status-var.
    (define (global-init-ccg type client-wrapset status-var)
      (let* ((wcp-type-name (type-get-name type))
             (equal-func (hashq-ref type 'wct:equal-func-name "NULL"))
             (print-func (hashq-ref type 'wct:print-func-name "NULL"))
             (mark-func (hashq-ref type 'wct:gc-mark-func-name "NULL"))
             (cleanup-func (hashq-ref type 'wct:cleanup-func-name "NULL"))
             (wct-init-ccg (hashq-ref type 'wct:init-ccg #f)))
        
        (list
         (cond
          ;; self-client.
          ((and client-wrapset (eq? client-wrapset wrapset))
           '())
          ;; regular client
          (client-wrapset
           (list
            "    " wct-var-name " = scm_c_eval_string(\"" wcp-type-name "\");\n"))
          ;; normal wrapset type code.
          (else
           (list
            "    " wct-var-name " = "
            "gw_wct_create("
            "\"" wcp-type-name "\", "
            equal-func ", "
            print-func ", "
            mark-func ", "
            cleanup-func ");\n"
            
            "  scm_c_define(\"" wcp-type-name "\", " wct-var-name ");\n")))
         
         (if wct-init-ccg
             (wct-init-ccg type client-wrapset)
             '()))))
    
    ;; This is so that any wrapset that depends on any wrapset that
    ;; wraps a wct will also have the header inserted...
    (if (not (hashq-ref wrapsets-w-wct-initializers wrapset #f))
        (begin          
          (wrapset-add-cs-declarations!
           wrapset
           (lambda (wrapset client-wrapset)
             "#include <g-wrap-wct.h>\n"))
          (hashq-set! wrapsets-w-wct-initializers wrapset #t)))

    
    (let ((wct (wrap-dynamic-type wrapset name-sym
                                     c-type-name c-const-type-name
                                     scm->c-ccg c->scm-ccg c-destructor
                                     'pointer)))
          
      (type-set-typespec-options-parser! wct typespec-options-parser)

      (type-set-global-declarations-ccg! wct global-declarations-ccg)
      (type-set-global-definitions-ccg! wct global-definitions-ccg)
      (type-set-global-initializations-ccg! wct global-init-ccg)
      
      (wrapset-add-guile-module-export! wrapset name-sym)
    
      wct)))

;; Are all these the overrides the "right thing"?  Is there a better
;; approach, and/or do we need them at all?

; (define-public (wct-set-global-ccg! t generator)
;   (hashq-set! t 'wct:global-ccg generator))

; (define-public (wct-set-init-ccg! t generator)
;   (hashq-set! t 'wct:init-ccg generator))

(define-public (wct-set-print-ccg! t generator)
  (hashq-set! t 'wct:print-func-name
              (string-append "gw__wct_print_for_"
                             (any-str->c-sym-str
                              (symbol->string (type-get-name type)))))
  (hashq-set! t 'wct:print-ccg generator))

(define-public (wct-set-equal?-ccg! t generator)
  (hashq-set! t 'wct:equal?-func-name
              (string-append "gw__wct_equal_p_for_"
                             (any-str->c-sym-str
                              (symbol->string (type-get-name type)))))
  (hashq-set! t 'wct:equal?-ccg generator))

(define-public (wct-set-gc-mark-ccg! t generator)
  (hashq-set! t 'wct:gc-mark-func-name
              (string-append "gw__wct_gc_mark_for_"
                             (any-str->c-sym-str
                              (symbol->string (type-get-name type)))))
  (hashq-set! t 'wct:gc-mark-ccg generator))

(define-public (wct-set-cleanup-c-rep-ccg! t generator)
  (hashq-set! t 'wct:cleanup-func-name
              (string-append "gw__wct_cleanup_for_"
                             (any-str->c-sym-str
                              (symbol->string (type-get-name type)))))
  (hashq-set! t 'wct:cleanup-ccg generator))



(let ((ws (new-wrapset "gw-wct")))

  (wrapset-set-guile-module! ws '(g-wrap gw-wct))

  (wrapset-depends-on ws "gw-standard")

  ;;(wrapset-add-cs-declarations!
  ;; ws
  ;; (lambda (wrapset client-wrapset)
  ;;    "#include <g-wrap-wct.h>\n"))
          
  (wrapset-add-cs-initializers!
   ws
   (lambda (wrapset client-wrapset status-var)
     (if (not client-wrapset)
         "gw_wct_initialize();\n"
         '())))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <wct> - wrapped c pointer type object
  (wrap-simple-type ws '<wct> "SCM"
                       '("gw_wct_p(" scm-var ")")
                       '(c-var " = " scm-var ";\n")
                       '(scm-var " = " c-var ";\n")
                       'pointer) ;; not accurate

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <wcp> - wrapped c pointer object
  (wrap-simple-type ws '<wcp> "SCM"
                       '("gw_wcp_p(" scm-var ")")
                       '(c-var " = " scm-var ";\n")
                       '(scm-var " = " c-var ";\n")
                       'pointer) ;; not accurate

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <void*> - wrapped c pointer object
  (wrap-as-wct ws '<void*> "void *" "const void *")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Wrapped functions...  

  (wrap-function
   ws
   'wct?
   '<bool> "gw_wct_p" '((<scm> obj))
   "Is obj a wct?")
  
  (wrap-function
   ws
   'wcp?
   '<bool> "gw_wcp_p" '((<scm> obj))
   "Is obj a wcp?")
  
  (wrap-function
   ws
   'wcp-is-of-type?
   '<bool> "gw_wcp_is_of_type_p" '((<wct> type) (<wcp> wcp))
   "Returns #f iff the given wcp is not of the type specified.  type must be a
g-wrap wrapped c type object, usually available via global bindings.  For
example (wcp-is-a? <void*> foo)")

  (wrap-function
   ws
   'wcp-coerce
   '<wcp> "gw_wcp_coerce" '((<wcp> wcp) (<wct> new-type))
   "Coerce the given wcp to new-type.  This can be dangerous, so be careful."))
