;; Copyright (C) 2004 Andreas Rottmann

(define-module (g-wrap)
  #:use-module (ice-9 optargs)
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
   argument-count
   return-type return-typespec
   generic-name 
   
   <gw-type>
   class-name
   c-type-name
   wrap-value-cg unwrap-value-cg destruct-value-cg
   pre-call-arg-cg pre-call-result-cg call-arg-cg post-call-result-cg
   post-call-arg-cg
   global-declarations-cg global-definitions-cg initializations-cg
   gen-c-tmp-name make-typespec
   
   <gw-typespec>
   type options
   
   <gw-value>
   var wrapped-var if-typespec-option
   
   <gw-param>
   number
   
   <gw-code>
   render
   
   <gw-wrapset>
   name language wrapsets-depended-on
   fold-types for-each-type lookup-type fold-functions
   arguments
   depends-on!
   add-type! add-constant! add-function!
   add-cs-before-includes! add-cs-global-declarator! add-cs-definer!
   add-cs-declarator! add-cs-initializer!
   wrap-function! wrap-constant!

   register-wrapset-class
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

(define-class <gw-type> (<gw-item>)
  (name #:getter name #:init-keyword #:name))

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

(define-generic c-type-name)

(define-generic pre-call-arg-cg)
(define-generic pre-call-result-cg)
(define-generic call-arg-cg)
(define-generic post-call-result-cg)
(define-generic post-call-arg-cg)

(define-class <gw-typespec> ()
  (type #:init-keyword #:type #:getter type)
  (options #:init-keyword #:options #:getter options #:init-value '()))

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

(define-class <gw-argument> ()
  (name #:getter name #:init-keyword #:name)
  (typespec #:init-keyword #:typespec #:getter typespec))

(define-method (type (arg <gw-argument>))
  (type (typespec arg)))
  
(define-method (argument-count (func <gw-function>))
  (length (slot-ref func 'arguments)))

(define-class <gw-constant> (<gw-item>)
  (name #:getter name #:init-keyword #:name)
  (value #:init-keyword #:value #:getter value)
  (typespec #:init-keyword #:typespec #:getter typespec))

(define-method (type (constant <gw-constant>))
  (type (typespec constant)))
  
(define-class <gw-value> ()
  (typespec #:getter typespec #:init-keyword #:typespec)
  (var #:getter var #:init-keyword #:var)
  (wrapped-var #:getter wrapped-var #:init-keyword #:wrapped-var))

(define-method (if-typespec-option (value <gw-value>) (option <symbol>)
                                   code1 . code2-opt)
  (let ((code2 (cond ((null? code2-opt) #f)
                     ((and (list? code2-opt) ((= length code2-opt) 1))
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

(define-class <gw-param> (<gw-value>)
  (number #:getter number #:init-keyword #:getter))

(define-class <gw-code> ())

(define-method (render (code <list>) (port <port>))
  (flatten-display code port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; <gw-wrapset>
;;;

(define-class <gw-wrapset> ()
  (name #:getter name #:init-keyword #:name)
  (language #:getter language #:init-keyword #:language)
  (dependencies #:getter wrapsets-depended-on #:init-value '())
  (items #:init-value '())
  (types #:init-value '())
  (functions #:init-value '())

  (cs-before-includes #:init-value '())
  (cs-global-declarators #:init-value '())
  (cs-definers #:init-value '())
  (cs-declarators #:init-value '())
  (cs-initializers #:init-value '()))

;;; Methods
(define-method (depends-on! (ws <gw-wrapset>) (dep-name <symbol>))
  (slot-set! ws 'dependencies
             (cons (get-wrapset (language ws) dep-name)
                   (slot-ref ws 'dependencies))))

(define-method (add-type! (ws <gw-wrapset>) (type <gw-type>))
  (slot-set! ws 'types (acons (name type) type (slot-ref ws 'types)))
  (slot-set! ws 'items (cons type (slot-ref ws 'items))))

(define-method (add-function! (ws <gw-wrapset>) (function <gw-function>))
  (slot-set! ws 'items (cons function (slot-ref ws 'items)))
  (slot-set! ws 'functions (cons function (slot-ref ws 'functions))))

(define-method (add-constant! (ws <gw-wrapset>) (constant <gw-constant>))
  (slot-set! ws 'items (cons constant (slot-ref ws 'items))))

(define-method (fold-types kons knil (ws <gw-wrapset>))
  (fold-right (lambda (pr result) (kons (cdr pr) result))
              knil
              (slot-ref ws 'types)))

(define-method (for-each-type proc (ws <gw-wrapset>))
  (fold-right (lambda (pr knil) (proc (cdr pr)))
              knil
              (slot-ref ws 'types)))
  
(define-method (lookup-type (wrapset <gw-wrapset>) (type-name <symbol>))
  (define (lookup wrapset type-name cont)
    (let* ((types-alist (slot-ref wrapset 'types))
           (ret (assq-ref types-alist type-name)))
      (if ret
          (cont ret)
          (begin
            (for-each
             (lambda (ws)
               (lookup ws type-name cont))
             (wrapsets-depended-on wrapset))
            (cont #f)))))
  
  (call-with-current-continuation
   (lambda (exit)
     (lookup wrapset type-name exit))))

(define-method (fold-functions kons knil (ws <gw-wrapset>))
  (fold kons knil(reverse (slot-ref ws 'functions))))

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
  (define (argspec spec)
    (if (not (and (list? spec) (= (length spec) 2)))
        (throw 'gw:bad-typespec
               (format #f "argument spec must be a two-element list (got ~S)"
                       spec)))
    (let ((ts (car spec)))
      (make <gw-argument>
        #:name (cadr spec)
        #:typespec (resolve-typespec wrapset ts))))
  
  (let loop ((specs argspecs) (args '()))
    (if (null? specs)
        (reverse args)
          (loop (cdr specs)
                (cons (argspec (car specs)) args)))))

(define-method (add-cs-before-includes! (ws <gw-wrapset>) (cg <procedure>))
  (slot-set! ws 'cs-before-includes
             (cons cg (slot-ref ws 'cs-before-includes))))

(define-method (add-cs-declarator! (ws <gw-wrapset>) (cg <procedure>))
  (slot-set! ws 'cs-declarators (cons cg (slot-ref ws 'cs-declarators))))

(define-method (add-cs-initializer! (ws <gw-wrapset>) (cg <procedure>))
  (slot-set! ws 'cs-initializers (cons cg (slot-ref ws 'cs-initializers))))

(define-method (add-cs-definer! (ws <gw-wrapset>) (cg <procedure>))
  (slot-set! ws 'cs-definers (cons cg (slot-ref ws 'cs-definers))))

(define-method (add-cs-global-declarator! (ws <gw-wrapset>) (cg <procedure>))
  (slot-set! ws 'cs-global-declarators
             (cons cg (slot-ref ws 'cs-global-declarators))))

;; High-level interface -- should move low-level stuff to core and
;; only offer this as API
(define-method (wrap-function! (wrapset <gw-wrapset>) . args)
  (let-keywords
      args #f (name returns c-name arguments description)
      (add-function!
       wrapset (make <gw-function>
                 #:name name
                 #:returns (resolve-typespec wrapset returns)
                 #:c-name c-name
                 #:arguments (resolve-arguments wrapset arguments)
                 #:description description))))

(define-method (wrap-constant! (wrapset <gw-wrapset>) . args)
  (let-keywords
   args #f (name type value description)
   (add-constant! wrapset (make <gw-constant>
                               #:name name
                               #:typespec (resolve-typespec wrapset type)
                               #:value value
                               #:description description))))

;; Wrapset registry

(define-generic initialize-wrapset)

(define *wrapset-registry* (make-hash-table))

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
            (set-cdr! entry wrapset)
            wrapset)))))

;;
;; Generation stuff
;;
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
              (flatten-display
               (list
                "if ((" error-var ").status != GW_ERR_NONE)\n"
                "  gw_handle_wrapper_error (&" error-var ",\n"
                "                            \"" wrapset-init-func "\",\n"
                "                            0);\n")
               port)))))
    
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

(define-method (generate-wrapset (lang <gw-language>)
                                 (wrapset <gw-wrapset>)
                                 (basename <string>))
  (let ((wrapset-source-name (string-append basename ".c"))
        (wrapset-name-c-sym (any-str->c-sym-str
                             (symbol->string (name wrapset)))))
    
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
        
        (for-each (lambda (cg)
                    (render (cg lang) port))
                  (reverse (slot-ref wrapset 'cs-global-declarators)))
        
        (for-each (lambda (cg)
                    (render (cg lang) port))
                  (reverse (slot-ref wrapset 'cs-definers)))
        
        (for-each
         (lambda (item)
           (render (global-declarations-cg lang wrapset item) port))
         (reverse (slot-ref wrapset 'items)))
        
        (for-each
         (lambda (item)
           (render (global-definitions-cg lang wrapset item) port))
         (reverse (slot-ref wrapset 'items)))

        (dsp-list
         (list
          "void\n"
          "gw_init_wrapset_" wrapset-name-c-sym "(void) {\n"
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
        
        (output-initializer-cgs wrapset lang
                                (reverse (slot-ref wrapset 'cs-initializers))
                                port)

        (output-initializer-cgs
         wrapset lang
         (map (lambda (item)
                (lambda (lang error-var)
                  (initializations-cg lang wrapset item error-var)))
              (reverse (slot-ref wrapset 'items)))
         port)
        
        (dsp-list
         (list
          "    gw_wrapset_initialized = 1;\n"
          "}\n"))))))



(define-method (global-declarations-cg (lang <gw-language>)
                                       (wrapset <gw-wrapset>)
                                       (type <gw-item>))
  '())

(define-method (global-definitions-cg (lang <gw-language>)
                                      (wrapset <gw-wrapset>)
                                      (type <gw-item>))
  '())

(define-method (initializations-cg (lang <gw-language>)
                                   (wrapset <gw-wrapset>)
                                   (item <gw-item>)
                                   error-var)
  '())
