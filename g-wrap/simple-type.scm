
(define-module (g-wrap simple-type)
  :use-module (g-wrap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple types.
;;;
;;; Simple types are simple :> They just need to specify their normal
;;; type name, their const type name, their type check function, their
;;; scm->c function, their c->scm function, and their destructor.  All
;;; the code will be generated automatically.  The newly created type
;;; will respond to 'const as a typespec option.
;;;
;;; The "functions" may only be a static tree of strings and symbols.
;;; Each symbol must be recognized and will be expanded to the proper
;;; value at runtime.  i.e.
;;;
;;;   (gw:wrap-simple-type '<gnc:guid-scm>
;;;                        "GUID"
;;;                        "const GUID"
;;;                        '("gnc_guid_p(" scm-var ")")
;;;                        '(c-var " = gnc_scm2guid(" scm-var ");\n")
;;;                        '(scm-var " = gnc_guid2scm(" c-var ");\n")
;;;

(define-public (gw:wrap-simple-type wrapset
                                    type-sym
                                    c-type-name
                                    type-check-form
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
            (error
             (string-append
              "g-wrap simple-type expected string for expansion "
              "while processing ~S from wrapset ~S\n.")
             type-sym
             (gw:wrapset-get-name wrapset)))))
     (else tree)))
  
  (let ((simple-type (gw:wrap-type wrapset type-sym)))
    
    (define (c-type-name-func typespec)
      c-type-name)
    
    (define (scm->c-ccg c-var scm-var typespec status-var)
      (let ((type-check-code (replace-syms type-check-form
                                           `((scm-var . ,scm-var))))
            (scm->c-code (replace-syms scm->c-form `((c-var . ,c-var)
                                                     (scm-var . ,scm-var)))))
        (list "if (!(" type-check-code "))"
              `(gw:error ,status-var type ,scm-var)
              "else {" scm->c-code "}")))

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

    (gw:type-set-c-type-name-func! simple-type c-type-name-func)
    (gw:type-set-scm->c-ccg! simple-type scm->c-ccg)
    (gw:type-set-c->scm-ccg! simple-type c->scm-ccg)
    (gw:type-set-pre-call-arg-ccg! simple-type pre-call-arg-ccg)
    (gw:type-set-call-ccg! simple-type call-ccg)
    (gw:type-set-post-call-result-ccg! simple-type post-call-result-ccg)
     
    simple-type))
