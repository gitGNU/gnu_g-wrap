(define-module (g-wrap dynamic-type)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:export (gw:wrap-dynamic-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic types.
;;;
;;; Dynamic types are simple. They just need to specify their normal
;;; type name, their const type name, their scm->c ccg, their c->scm
;;; ccg, and their c-destructor. All the code will be generated
;;; automatically.  The newly created type will respond to 'const as a
;;; typespec option as well as to 'callee-owned and 'caller-owned.
;;; Furthermore, functions only having dynamic type arguments and
;;; return type can be called dynamically, via libffi.
;;;

(define-method (gw:wrap-dynamic-type (wrapset <top>)
                                     (type-sym <symbol>)
                                     (c-type-name <string>)
                                     (c-const-type-name <string>)
                                     (scm->c-ccg <procedure>)
                                     (c->scm-ccg <procedure>)
                                     (c-destructor <procedure>)
                                     (c-typedef <symbol>))
  
  (let ((dynamic-type (gw:wrap-type wrapset type-sym)))
    
    (define (c-type-name-func typespec)
      (if (memq 'const (gw:typespec-get-options typespec))
          c-const-type-name
          c-type-name))

    (define (typespec-options-parser options-form wrapset)
      (let ((remainder options-form))
        (set! remainder (delq 'const remainder))
        (if (and (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder))
            (throw 'gw:bad-typespec
                   "Bad dynamic-type options form (caller and callee owned!)."
                   options-form))
        (if (not (or (memq 'caller-owned remainder)
                     (memq 'callee-owned remainder)))
            (throw 'gw:bad-typespec
                   (format #t "Bad dynamic-type options form for type ~A (must be caller or callee owned!)." type-sym)
                   options-form))
        (set! remainder (delq 'caller-owned remainder))
        (set! remainder (delq 'callee-owned remainder))
        (if (null? remainder)
            options-form
            (throw 'gw:bad-typespec
                   "Bad dynamic-type options form - spurious options: "
                   remainder))))
    
    (define (c-typespec-ccg ts)
      (let ((options (gw:typespec-get-options ts)))
        (cond ((memq 'caller-owned options) "GW_TYPESPEC_CALLER_OWNED")
              ((memq 'callee-owned options) "GW_TYPESPEC_CALLEE_OWNED")
              (else (error "bogus typespec options")))))
    
    (define (pre-call-arg-ccg param status-var)
      (let* ((scm-name (gw:param-get-scm-name param))
             (c-name (gw:param-get-c-name param))
             (typespec (gw:param-get-typespec param)))
        (list
         (scm->c-ccg c-name scm-name typespec status-var)
         "if (" `(gw:error? ,status-var type) ")"
         `(gw:error ,status-var arg-type)
         "else if (" `(gw:error? ,status-var range) ")"
         `(gw:error ,status-var arg-range))))
    
    (define (call-ccg result func-call-code status-var)
      (list (gw:result-get-c-name result) " = " func-call-code ";\n"))

    (define (post-call-result-ccg result status-var)
      (let* ((scm-name (gw:result-get-scm-name result))
             (c-name (gw:result-get-c-name result))
             (typespec (gw:result-get-typespec result)))
        (c->scm-ccg scm-name c-name typespec status-var)))

    (gw:type-set-c-type-name-func! dynamic-type c-type-name-func)
    (gw:type-set-typespec-options-parser! dynamic-type typespec-options-parser)
    
    (gw:type-set-scm->c-ccg! dynamic-type scm->c-ccg)
    (gw:type-set-c->scm-ccg! dynamic-type c->scm-ccg)
    (gw:type-set-c-destructor! dynamic-type c-destructor)
    (gw:type-set-pre-call-arg-ccg! dynamic-type pre-call-arg-ccg)
    (gw:type-set-call-ccg! dynamic-type call-ccg)
    (gw:type-set-post-call-result-ccg! dynamic-type post-call-result-ccg)
    (gw:type-set-dynamic! dynamic-type c-typedef c-typespec-ccg)
     
    dynamic-type))

