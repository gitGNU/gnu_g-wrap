;; Copyright (C) 2004 Andreas Rottmann

(define-module (g-wrap)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (g-wrap util)
  
  #:export
  (<gw-language>

   <gw-item>
   description
   
   <gw-constant>
   value typespec 

   <gw-function>
   c-name
   argument-count arguments argument-types
   return-type return-typespec
   generic-name 
   
   <gw-type>
   class-name needs-result-var?
   wrap-value-cg unwrap-value-cg destruct-value-cg
   pre-call-arg-cg pre-call-result-cg call-arg-cg post-call-result-cg
   post-call-arg-cg call-cg
   
   global-declarations-cg global-definitions-cg initializations-cg
   
   client-global-declarations-cg client-global-definitions-cg
   client-initializations-cg
   
   gen-c-tmp-name make-typespec
   
   <gw-typespec>
   type options c-type-name
   
   <gw-value>
   var wrapped-var if-typespec-option
   
   <gw-argument>
   visible?

   <gw-param>
   number
   
   <gw-code>
   render no-op?
   has-error-form? expand-special-forms
   
   <gw-wrapset>
   name language wrapsets-depended-on
   fold-types for-each-type lookup-type fold-functions
   depends-on!
   add-type! add-constant! add-function!
   defines-generic?
   
   add-cs-before-includes! add-cs-global-declarator! add-cs-definer!
   add-client-cs-before-includes! add-client-cs-global-declarator! 
   add-cs-declarator! add-cs-initializer! add-cs-init-finalizer!
   wrap-function! wrap-constant!

   initialize-wrapset
   
   generate-wrapset
   ))

(define-class <gw-language> ()
  (description #:getter description #:init-keyword #:description))

;; An <gw-item> is "something" that shows up in the generated
;; wrapper. The following generics are invoked on all items:
;;
;; global-declarations-cg
;; global-definitions-cg
;; initializations-cg
;;
(define-class <gw-item> ()
  (description #:getter description
               #:init-keyword #:description
               #:init-value #f))

 ;; Upgrade the GOOPS class-name procedure
(define class-name (ensure-accessor class-name))

;;;
;;; Types
;;;

(define-class <gw-type> (<gw-item>)
  (name #:getter name #:init-keyword #:name)
  (class-name #:accessor class-name
              #:init-keyword #:class-name
              #:init-value #f)
  (needs-result-var? #:getter needs-result-var?
                     #:init-keyword #:needs-result-var?
                     #:init-value #t))

(define-method (write (type <gw-type>) port)
  (let ((class (class-of type)))
    (display "#<" port)
    (display (class-name class) port)
    (display #\space port)
    (display (name type) port)
    (display #\> port)))

(define-method (gen-c-tmp-name (type <gw-type>) (suffix <string>))
  (gen-c-tmp (string-append (any-str->c-sym-str
                             (symbol->string
                              (name type))) "_" suffix)))

(define-method (make-typespec (type <gw-type>) (options <list>))
  (if (null? options)
      (make <gw-typespec> #:type type)
      (throw
       'gw:bad-typespec #f
       (format #f "bad typespec ~S -- a typespec may only be a symbol by default" options))))

;;;
;;; Values
;;;

(define-class <gw-value> ()
  (typespec #:getter typespec #:init-keyword #:typespec)
  (var #:getter var #:init-keyword #:var)
  (wrapped-var #:getter wrapped-var #:init-keyword #:wrapped-var))

(define-method (type (value <gw-value>))
  (type (typespec value)))

(define-method (if-typespec-option (value <gw-value>) (option <symbol>)
                                   code1 . code2-opt)
  (let ((code2 (cond ((null? code2-opt) #f)
                     ((and (list? code2-opt) (= (length code2-opt) 1))
                      (car code2-opt))
                     (else (error "bogus parameters")))))
    (if (memq option (options (typespec value)))
        code1
        (if code2 code2 '()))))

(define-generic wrap-value-cg)
(define-generic unwrap-value-cg)
(define-generic destruct-value-cg)

(define-method (destruct-value-cg (lang <gw-language>)
                                  (type <gw-type>)
                                  (value <gw-value>)
                                  error-var)
  '())

(define-method (pre-call-arg-cg (lang <gw-language>)
                                (type <gw-type>)
                                (param <gw-value>)
                                status-var)
  (list
   (unwrap-value-cg lang type param status-var)
   "if (" `(gw:error? ,status-var type) ")"
   `(gw:error ,status-var arg-type)
   "else if (" `(gw:error? ,status-var range) ")"
   `(gw:error ,status-var arg-range)))


(define-method (call-arg-cg (lang <gw-language>) (type <gw-type>)
                            (value <gw-value>))
  (list (var value)))

(define-method (call-cg (lang <gw-language>) (type <gw-type>)
                        (result <gw-value>) func-call-code error-var)
  (list (var result) " = " func-call-code ";\n"))

(define-method (post-call-result-cg (lang <gw-language>)
                                    (type <gw-type>)
                                    (result <gw-value>)
                                    status-var)
  (list
   (wrap-value-cg lang type result status-var)
   (destruct-value-cg lang type result status-var)))


(define-method (post-call-arg-cg (lang <gw-language>) (type <gw-type>)
                                 (value <gw-value>) error-var)
  (destruct-value-cg lang type result status-var))

;;;

(define-class <gw-param> (<gw-value>)
  (number #:getter number #:init-keyword #:number))

(define-class <gw-typespec> ()
  (type #:init-keyword #:type #:getter type)
  (options #:init-keyword #:options #:getter options #:init-value '()))

(define-generic c-type-name)

;;;
;;; Functions
;;;

(define-class <gw-function> (<gw-item>)
  (name #:getter name #:init-keyword #:name)
  (c-name #:getter c-name #:init-keyword #:c-name)
  (returns #:getter return-typespec #:init-keyword #:returns)
  (arguments #:getter arguments #:init-keyword #:arguments)
  (generic-name #:getter generic-name
                #:init-keyword #:generic-name
                #:init-value #f))

(define-method (return-type (function <gw-function>))
  (type (return-typespec function)))

(define-method (argument-count (func <gw-function>))
  (length (slot-ref func 'arguments)))

(define-method (argument-types (func <gw-function>))
  (map type (slot-ref func 'arguments)))

;;; Function (formal) arguments

(define-class <gw-argument> ()
  (typespec #:getter typespec #:init-keyword #:typespec)
  (name #:getter name #:init-keyword #:name))

(define-method (visible? (arg <gw-argument>))
  #t) ;; FIXME: implement in terms of type visibility

(define-method (type (arg <gw-argument>))
  (type (typespec arg)))

;;; Constants

(define-class <gw-constant> (<gw-item>)
  (name #:getter name #:init-keyword #:name)
  (value #:init-keyword #:value #:getter value)
  (typespec #:init-keyword #:typespec #:getter typespec))

(define-method (type (constant <gw-constant>))
  (type (typespec constant)))

;;;
;;; Code (currently nested string lists)
;;;
(define-class <gw-code> ())

(define-method (render (code <list>) (port <port>))
  (flatten-display code port))

(define-method (no-op? (code <list>)) (null? list))


;;;
;;; Wrapsets
;;;

;;; Metaclass - handles wrapset registry
(define-class <gw-wrapset-class> (<class>))

;; This should be in goops.scm, really
(define (class-supers c)
  (letrec ((allsubs (lambda (c)
                      (cons c (mapappend allsubs
                                         (class-direct-supers c))))))
    (list2set (cdr (allsubs c)))))

(define-method (initialize (class <gw-wrapset-class>) initargs)
  (next-method)
  (let-keywords
   initargs #t (language id)
   (if (not language)
       (set! language
             (any (lambda (c) (class-slot-ref c 'language))
                           (filter
                            (lambda (c) (not (eq? <object> c)))
                            (class-direct-supers class)))))
   (class-slot-set! class 'language language)

   (if (and language id)
       (register-wrapset-class language id class))))

(define-class <gw-wrapset> ()
  (name #:getter name #:init-keyword #:name)
  (language #:getter language #:init-keyword #:language
            #:allocation #:each-subclass)
  
  (dependencies #:getter wrapsets-depended-on #:init-value '())
  (items #:init-value '())
  (types #:init-value '())
  (type-hash #:init-form (make-hash-table 53))
  (functions #:init-value '())
  
  (generic-hash #:init-form (make-hash-table 31))
  
  (function-class #:init-keyword #:function-class #:init-value <gw-function>)
  
  (cs-before-includes #:init-value '())
  (cs-client-before-includes #:init-value '())
  (cs-global-declarators #:init-value '())
  (cs-client-global-declarators #:init-value '())
  (cs-definers #:init-value '())
  (cs-declarators #:init-value '())
  (cs-initializers #:init-value '())
  (cs-init-finalizers #:init-value '())

  #:metaclass <gw-wrapset-class>)

;;; Methods
(define-method (depends-on! (ws <gw-wrapset>) (dep-name <symbol>) . deps)
  (slot-set! ws 'dependencies
             (append!
              (map (lambda (name)
                     (get-wrapset (language ws) name))
                   (cons dep-name deps))
              (slot-ref ws 'dependencies))))

(define-method (add-type! (ws <gw-wrapset>) (type <gw-type>))
  (slot-set! ws 'types (cons type (slot-ref ws 'types)))
  (slot-set! ws 'items (cons type (slot-ref ws 'items)))
  (if (hashq-ref (slot-ref ws 'type-hash) (name type))
      (error "trying to double-register type ~S in ~S" type ws))
  (hashq-set! (slot-ref ws 'type-hash) (name type) type))

(define-method (add-function! (ws <gw-wrapset>) (function <gw-function>))
  (slot-set! ws 'items (cons function (slot-ref ws 'items)))
  (slot-set! ws 'functions (cons function (slot-ref ws 'functions)))
  (if (generic-name function)
      (let ((handle (hashq-create-handle! (slot-ref ws 'generic-hash)
                                          (generic-name function) '())))
        (set-cdr! handle (cons function (cdr handle))))))

(define-method (add-constant! (ws <gw-wrapset>) (constant <gw-constant>))
  (slot-set! ws 'items (cons constant (slot-ref ws 'items))))

(define-method (fold-types kons knil (ws <gw-wrapset>))
  (fold-right kons knil (slot-ref ws 'types)))

(define-method (for-each-type proc (ws <gw-wrapset>))
  (for-each proc (reverse (slot-ref ws 'types))))

(define (wrapset-lookup-recursive wrapset slot name)
  (let ((ret (hashq-ref (slot-ref wrapset slot) name)))
    (or ret (any
             (lambda (ws)
               (wrapset-lookup-recursive ws slot name))
             (wrapsets-depended-on wrapset)))))

(define-method (lookup-type (wrapset <gw-wrapset>) (type-name <symbol>))
  (wrapset-lookup-recursive wrapset 'type-hash type-name))

(define-method (lookup-generic (wrapset <gw-wrapset>) (generic-name <symbol>))
  (wrapset-lookup-recursive wrapset 'generic-hash generic-name))

(define-method (defines-generic? (wrapset <gw-wrapset>) (name <symbol>))
  (not (any (lambda (ws) (lookup-generic ws name))
            (wrapsets-depended-on wrapset))))

(define-method (fold-functions kons knil (ws <gw-wrapset>))
  (fold kons knil (reverse (slot-ref ws 'functions))))

(define-method (for-each-function proc (ws <gw-wrapset>))
  (for-each proc (reverse (slot-ref ws 'functions))))

(define (resolve-typespec wrapset spec)
  (let* ((form (cond
                ((symbol? spec) (list spec))
                ((list? spec) spec)
                (else (throw
                       'gw:bad-typespec
                       (format #f "neither list nor symbol (~S)" spec)))))
         (type (lookup-type wrapset (car form))))
    (if type
        (make-typespec type (cdr form))
        (throw
         'gw:bad-typespec
         (format #f "no type ~S in wrapset ~S" (car form) (name wrapset))))))

(define (resolve-arguments wrapset argspecs)
  (define (argument i spec)
    (if (not (and (list? spec) (= (length spec) 2)))
        (throw 'gw:bad-typespec
               (format #f "argument spec must be a two-element list (got ~S)"
                       spec)))
    (let ((ts (car spec)))
      (make <gw-argument>
        #:number i
        #:name (cadr spec)
        #:typespec (resolve-typespec wrapset ts))))
  
  (let loop ((i 0) (specs argspecs) (args '()))
    (if (null? specs)
        (reverse args)
          (loop (+ i 1) (cdr specs)
                (cons (argument i (car specs)) args)))))

(define-method (add-cs-before-includes! (ws <gw-wrapset>) (cg <procedure>))
  (slot-set! ws 'cs-before-includes
             (cons cg (slot-ref ws 'cs-before-includes))))

(define-method (add-cs-declarator! (ws <gw-wrapset>) (cg <procedure>))
  (slot-set! ws 'cs-declarators (cons cg (slot-ref ws 'cs-declarators))))

(define-method (add-cs-initializer! (ws <gw-wrapset>) (cg <procedure>))
  (slot-set! ws 'cs-initializers (cons cg (slot-ref ws 'cs-initializers))))

(define-method (add-cs-definer! (ws <gw-wrapset>) (cg <procedure>))
  (slot-set! ws 'cs-definers (cons cg (slot-ref ws 'cs-definers))))

(define-method (add-cs-init-finalizer! (ws <gw-wrapset>) (cg <procedure>))
  (slot-set! ws 'cs-init-finalizers
             (cons cg (slot-ref ws 'cs-init-finalizers))))

(define-method (add-cs-global-declarator! (ws <gw-wrapset>) (cg <procedure>))
  (slot-set! ws 'cs-global-declarators
             (cons cg (slot-ref ws 'cs-global-declarators))))

(define-method (add-client-cs-global-declarator! (ws <gw-wrapset>)
                                                 (cg <procedure>))
  (slot-set! ws 'cs-client-global-declarators
             (cons cg (slot-ref ws 'cs-client-global-declarators))))

(define-method (add-client-cs-before-includes! (ws <gw-wrapset>)
                                               (cg <procedure>))
  (slot-set! ws 'cs-client-before-includes
             (cons cg (slot-ref ws 'cs-client-before-includes))))

;; High-level interface -- should move low-level stuff to core and
;; only offer this as API
(define-method (wrap-function! (wrapset <gw-wrapset>) . args)
  ;;(format #t "wrapping ~S\n" args)
  (let-keywords
      args #f (name returns c-name arguments description generic-name)
      (add-function!
       wrapset (make (slot-ref wrapset 'function-class)
                 #:name name
                 #:returns (resolve-typespec wrapset returns)
                 #:c-name c-name
                 #:arguments (resolve-arguments wrapset arguments)
                 #:description description
                 #:generic-name generic-name))))

(define-method (wrap-constant! (wrapset <gw-wrapset>) . args)
  (let-keywords
   args #f (name type value description)
   (add-constant! wrapset (make <gw-constant>
                               #:name name
                               #:typespec (resolve-typespec wrapset type)
                               #:value value
                               #:description description))))

;;;
;;; Wrapset registry
;;;

(define-generic initialize-wrapset)

(define *wrapset-registry* (make-hash-table 7))

(define-method (register-wrapset-class (lang <gw-language>) (name <symbol>)
                                       (class <class>))
  (let ((key (cons lang name)))
    (if (hash-ref *wrapset-registry* key)
        (error "tried to re-register wrapset class" lang name class *wrapset-registry*))
    (hash-set! *wrapset-registry* key (cons class #f))))

(define-method (get-wrapset (lang <gw-language>) (name <symbol>))
  (let ((handle (hash-get-handle *wrapset-registry* (cons lang name))))
    (if (not handle)
        (error "no wrapset registered for" lang name))
    (let ((entry (cdr handle)))
      (if (cdr entry)
          (cdr entry)
          (let ((wrapset (make (car entry) #:name name #:language lang)))
            ;;(format #t "Instantiating ~A for ~A\n" name lang)
            (set-cdr! entry wrapset)
            wrapset)))))


;;;
;;; Generation
;;;
(define (output-initializer-cgs wrapset lang cgs port)
  (let* ((error-var (gen-c-tmp "error_var"))
         (wrapset-name (name wrapset))
         (wrapset-name-c-sym (any-str->c-sym-str
                              (symbol->string wrapset-name)))
         (wrapset-init-func (string-append "gw_init_wrapset_"
                                           wrapset-name-c-sym)))

    (define (output-initializer-cg cg)
      (let ((code (cg lang error-var)))
        (if (not (null? code))
            (begin
              (render (expand-special-forms code #f '(type range memory misc))
                      port)
              (if (has-error-form? code)
                  (flatten-display
                   (list
                    "if ((" error-var ").status != GW_ERR_NONE)\n"
                    "  gw_handle_wrapper_error (gw__arena, &" error-var ",\n"
                    "                            \"" wrapset-init-func "\",\n"
                    "                            0);\n")
                   port))))))
    
    (flatten-display
     (list "{\n"
           "  GWError " error-var ";\n"
           "   " error-var ".status = GW_ERR_NONE;\n"
           "   " error-var ".data = SCM_UNSPECIFIED;\n"
           "   " error-var ".message = NULL;\n"
           "   (void) " error-var ";\n")
     port)

    (for-each (lambda (cg) (output-initializer-cg cg)) cgs)

    (display "}\n" port)))

(define-method (generate-wrapset (lang <gw-language>)
                                 (name <symbol>)
                                 (basename <string>))
  (generate-wrapset lang (get-wrapset lang name) basename))

(define (compute-client-types ws)
  (let ((client-type-hash (make-hash-table 13))
        (my-types (slot-ref ws 'type-hash)))
    (for-each
     (lambda (func)
       (for-each
        (lambda (type)
          (let ((type-name (name type)))
            ;;(format #t "considering ~S as client type\n" type)
            (if (not (hashq-ref my-types type-name))
                (hashq-set! client-type-hash type-name type))))
        (cons (return-type func) (argument-types func))))
       (slot-ref ws 'functions))
    (hash-fold (lambda (key val rest) (cons val rest)) '() client-type-hash)))

(define-method (generate-wrapset (lang <gw-language>)
                                 (wrapset <gw-wrapset>)
                                 (basename <string>))
  (let ((wrapset-source-name (string-append basename ".c"))
        (wrapset-name-c-sym (any-str->c-sym-str
                             (symbol->string (name wrapset))))
        (client-types (compute-client-types wrapset)))

    ;;(format #t "client types: ~S\n" client-types)
    
    (call-with-output-file wrapset-source-name
      (lambda (port)

        (define (dsp-list lst)
          (for-each (lambda (s) (display s port)) lst))

        (dsp-list
         (list
          "/* Generated by G-Wrap-TNG: an experimental wrapper engine */\n"
          "\n"))
        
        (for-each (lambda (cg)
                    (render (cg lang) port))
                  (reverse (slot-ref wrapset 'cs-before-includes)))

        (for-each
         (lambda (ws)
           (for-each (lambda (cg)
                       (render (cg lang) port))
                     (reverse (slot-ref ws 'cs-client-before-includes))))
         (wrapsets-depended-on wrapset))
        
        (for-each
         (lambda (ws)
           (for-each (lambda (cg)
                       (render (cg lang) port))
                     (reverse (slot-ref ws 'cs-client-global-declarators))))
         (wrapsets-depended-on wrapset))
        
        (for-each (lambda (cg)
                    (render (cg lang) port))
                  (reverse (slot-ref wrapset 'cs-global-declarators)))
        
        (dsp-list
         (list "void gw_init_wrapset_" wrapset-name-c-sym "(GWLangArena);\n"))
        
        (for-each (lambda (cg)
                    (render (cg lang) port))
                  (reverse (slot-ref wrapset 'cs-definers)))

        ;; Global client declarations and definitions
        (for-each
         (lambda (type)
           (render (client-global-declarations-cg lang wrapset type) port))
         client-types)
        
        (for-each
         (lambda (type)
           (render (client-global-definitions-cg lang wrapset type) port))
         client-types)

        ;; Global declarations and definitions
        (let ((items (reverse (slot-ref wrapset 'items))))
          (for-each
           (lambda (item)
             (render (global-declarations-cg lang wrapset item) port))
           items)
        
          (for-each
           (lambda (item)
             (render (global-definitions-cg lang wrapset item) port))
           items))

        ;; The initialization function
        (dsp-list
         (list
          "void\n"
          "gw_init_wrapset_" wrapset-name-c-sym "(GWLangArena gw__arena) {\n"
          "  static int gw_wrapset_initialized = 0;\n"
          "\n"))

        (for-each (lambda (cg)
                    (render (cg lang) port))
                  (reverse (slot-ref wrapset 'cs-declarators)))

        (dsp-list
         (list
          "  if(gw_wrapset_initialized)\n"
          "   return;\n"
          "\n"))
        
        (output-initializer-cgs
         wrapset lang
         (append
          (reverse (slot-ref wrapset 'cs-initializers))
          (map (lambda (item)
                 (lambda (lang error-var)
                   (client-initializations-cg lang wrapset item error-var)))
               client-types)
          (map (lambda (item)
                (lambda (lang error-var)
                  (initializations-cg lang wrapset item error-var)))
              (receive (types others)
                  (partition! (lambda (item)
                                (is-a? item <gw-type>))
                              (reverse (slot-ref wrapset 'items)))
                (append! types others)))
          (slot-ref wrapset 'cs-init-finalizers))
          port)
        
        (dsp-list
         (list
          "    gw_wrapset_initialized = 1;\n"
          "}\n"))))))



(define-method (global-declarations-cg (lang <gw-language>)
                                       (wrapset <gw-wrapset>)
                                       (item <gw-item>))
  '())

(define-method (client-global-declarations-cg (lang <gw-language>)
                                              (wrapset <gw-wrapset>)
                                              (item <gw-item>))
  '())

(define-method (global-definitions-cg (lang <gw-language>)
                                      (wrapset <gw-wrapset>)
                                      (item <gw-item>))
  '())

(define-method (client-global-definitions-cg (lang <gw-language>)
                                              (wrapset <gw-wrapset>)
                                              (item <gw-item>))
  '())

(define-method (initializations-cg (lang <gw-language>)
                                   (wrapset <gw-wrapset>)
                                   (item <gw-item>)
                                   error-var)
  '())


(define-method (client-initializations-cg (lang <gw-language>)
                                          (wrapset <gw-wrapset>)
                                          (item <gw-item>)
                                          error-var)
  '())


;;;
;;; Code expansion
;;;

;;(gw:error? status-var ...)
;;(gw:error? status-var alloc bad-arg)
;;(gw:error status-var alloc)

;; arg-type arg-range memory misc

;; (gw:wrap-value m 'gtk:green '<gw:int> "GTK_GREEN")

;; (gw:error status-var type ...)

(define (expand-gw-error args param allowed-errors top-form)
  ;; args will be something like (status-var err-sym)

  (if (or (null? args) (null? (cdr args)))
      (error "not enough args to gw:error"))
  (if (not (memq (cadr args) allowed-errors))
      (scm-error 'misc-error "gw:expand-gw-error"
                 "gw:error type \"~A\" not allowed in ~S"
                 (list (cadr args) top-form) 
                 #f))

  (let ((error-var (car args)))
    (set! args (cdr args))
    (list
     "{\n"
     
     (case (car args)
       ((misc)
        ;; (list 'gw:error 'misc msg format-args)
        (if (not (= 3 (length args))) (error "bad call to (gw:error 'misc ...)"))
        (list
         "   (" error-var ").status = GW_ERR_MISC;\n"
         "   (" error-var ").message = " (list-ref args 1) ";\n"
         "   (" error-var ").data = " (list-ref args 2) ";\n"))
       ((memory)
        ;; (list 'gw:error 'memory) 
        (if (not (= 1 (length args)))
            (error "bad call to (gw:error 'memory ...)"))
        (list
         "   (" error-var ").status = GW_ERR_ARG_MEMORY;\n"))
       ((range)
        ;; (list 'gw:error 'range scm-item-out-of-range)
        (if (not (= 2 (length args)))
            (error "bad call to (gw:error 'range ...)"))
        (list
         "   (" error-var ").status = GW_ERR_ARG_TYPE;\n"
         "   (" error-var ").data = " (cadr args) ";\n"))
       ((type)
        ;; (list 'gw:error 'type scm-bad-type-item)
        (if (not (= 2 (length args)))
            (error "bad call to (gw:error 'type ...)"))
        (list
         "   (" error-var ").status = GW_ERR_ARG_TYPE;\n"
         "   (" error-var ").data = " (cadr args) ";\n"))
       ((argc)
        ;; (list 'gw:error 'argc)
        (if (not (= 1 (length args))) (error "bad call to (gw:error 'argc ...)"))
        (list
         "   (" error-var ").status = GW_ERR_ARGC;\n"))
       ((arg-type)
        (if (not (= 1 (length args)))
            (error "bad call to (gw:error 'arg-type ...)"))
        (list
         "   (" error-var ").status = GW_ERR_ARG_TYPE;\n"
         "   (" error-var ").data = " (wrapped-var param) ";\n"))
       ((arg-range)
        (if (not (= 1 (length args)))
            (error "bad call to (gw:error 'arg-range ...)"))
        (list
         "   (" error-var ").status = GW_ERR_ARG_RANGE;\n"
         "   (" error-var ").data = " (wrapped-var param) ";\n"))
       (else
        (error "unexpected error type in gw:error")))
     
     (if param
         (list "   goto gw__post_call_arg_" (number param) ";\n")
         "")
         
     "}\n")))

(define (has-error-form? tree)
  (if (and (list? tree) (not (null? tree)))
      (if (eq? (car tree) 'gw:error)
          #t
          (any has-error-form? tree))
      #f))
   
(define (expand-special-forms tree param allowed-errors)
  (define (expand-helper tree param allowed-errors top-form)
    (cond
     ((null? tree) tree)
     ((list? tree)
      (case (car tree)
        ((gw:error?)
         (cond
          ((= 2 (length tree))
           (let ((error-var (list-ref tree 1)))
             (list "((" error-var ").status != GW_ERR_NONE)")))
          ((= 3 (length tree))
           (let ((error-var (list-ref tree 1))
                 (err-sym
                  (case (list-ref tree 2)
                    ((misc) "GW_ERR_MISC")
                    ((memory) "GW_ERR_MEMORY")
                    ((range) "GW_ERR_RANGE")
                    ((type) "GW_ERR_TYPE")
                    ((argc) "GW_ERR_ARGC")
                    ((arg-range) "GW__ARG_RANGE")
                    ((arg-type) "GW__ARG_TYPE")
                    (else (error "improper error type given to gw:error?: "
                                 (list-ref tree 2))))))
             (list "((" error-var ").status == " err-sym ")")))
          (else
           (error "improper use of gw:error?"))))
        ((gw:error)
         (expand-gw-error (cdr tree) param allowed-errors top-form))
        (else
         (map
          (lambda (elt) (expand-helper elt param allowed-errors top-form))
          tree))))
     (else tree)))
  (expand-helper tree param allowed-errors tree))


