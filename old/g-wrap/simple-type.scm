
(define-module (g-wrap simple-type)
  #:use-module (g-wrap)
  #:use-module (g-wrap dynamic-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple types.
;;;
;;; Simple types are simple :> They just need to specify their type
;;; name, their type check function, their scm->c function, their
;;; c->scm function.  All the code will be generated automatically.
;;;
;;; A simple type is implemented as a dynamic type wich is always
;;; callee-owned (since simple type are assumed to be converted
;;; completly, i.e. the SCM value has no reference to the C value).
;;;
;;; The "functions" may only be a static tree of strings and symbols.
;;; Each symbol must be recognized and will be expanded to the proper
;;; value at runtime.  i.e.
;;;
;;;   (gw:wrap-simple-type '<gnc:guid-scm>
;;;                        "GUID"
;;;                        '("gnc_guid_p(" scm-var ")")
;;;                        '(c-var " = gnc_scm2guid(" scm-var ");\n")
;;;                        '(scm-var " = gnc_guid2scm(" c-var ");\n")
;;;

(define-public (gw:wrap-simple-type wrapset
                                    type-sym
                                    c-type-name
                                    type-check-form
                                    scm->c-form
                                    c->scm-form
                                    c-typedef)
  
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
              "g-wrap dynamic-type expected string for expansion "
              "while processing ~S from wrapset ~S\n.")
             type-sym
             (gw:wrapset-get-name wrapset)))))
     (else tree)))
  
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

  (define (c-destructor c-var typespec status-var force?)
    '())
  
  (let ((simple-type (gw:wrap-dynamic-type wrapset
                                           type-sym
                                           c-type-name
                                           c-type-name
                                           scm->c-ccg
                                           c->scm-ccg
                                           c-destructor
                                           c-typedef)))
    
    (define (typespec-options-parser options-form wrapset)
      (let ((remainder options-form))
        (set! remainder (delq 'const remainder))
        (if (null? remainder)
            (cons 'callee-owned options-form)
            (throw 'gw:bad-typespec
                   "Bad simple-type options form - spurious options: "
                   remainder))))

    
    (gw:type-set-typespec-options-parser! simple-type typespec-options-parser)
    
    simple-type))

