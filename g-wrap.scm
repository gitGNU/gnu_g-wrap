;; Copyright (C) 2004 Andreas Rottmann

(define-module (g-wrap)
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
   
   <gw-type>
   class-name
   c-type-name
   wrap-value-cg unwrap-value-cg destruct-value-cg
   pre-call-arg-cg pre-call-result-cg call-arg-cg post-call-result-cg
   post-call-arg-cg
   global-declarations-cg global-definitions-cg initializations-cg
   gen-c-tmp-name make-typespec
   
   <gw-typespec>
   type
   
   <gw-value>
   var wrapped-var
   
   <gw-param>
   number
   
   <gw-code>
   render
   
   <gw-wrapset>
   name wrapsets-depended-on functions
   fold-types for-each-type lookup-type
   depends-on!
   add-type! add-constant! add-function!
   add-cs-before-includes! add-cs-global-declarator! add-cs-definer!
   add-cs-declarator! add-cs-initializer!
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
  (class-name #:getter class-name #:init-keyword #:class-name
              #:init-value #f))

(define-method (gen-c-tmp-name (type <gw-type>) (name <string>))
  (gen-c-tmp (string-append (any-str->c-sym-str (c-type-name type)) "_" name)))

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
  (type #:init-keyword #:type #:getter type))

(define-class <gw-function> (<gw-item>)
  (name #:getter name #:init-keyword #:name)
  (c-name #:init-keyword #:c-name)
  (returns #:init-keyword #:returns)
  (arguments #:init-keyword #:arguments)
  (generic-name #:getter generic-name
                #:init-keyword #:generic-name
                #:init-value #f))

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

(define-class <gw-param> (<gw-value>)
  (number #:getter number #:init-keyword #:getter))

(define-generic wrap-value-cg)
(define-generic unwrap-value-cg)
(define-generic destruct-value-cg)

(define-method (destruct-value-cg (lang <gw-language>)
                                  (type <gw-type>)
                                  (value <gw-value>)
                                  error-var)
  '())

(define-class <gw-code> ())

(define-method (render (code <list>) (port <port>))
  (flatten-display code port))

(define-class <gw-wrapset> ()
  (name #:getter name #:init-keyword #:name)
  (dependencies #:getter wrapsets-depended-on #:init-value '())
  (items #:init-value '())
  (types #:init-value '())

  (cs-before-includes #:init-value '())
  (cs-global-declarators #:init-value '())
  (cs-definers #:init-value '())
  (cs-declarators #:init-value '())
  (cs-initializers #:init-value '()))

(define-method (depends-on! (ws <gw-wrapset>) (dependency <gw-wrapset>))
  (slot-set! ws 'dependencies (cons dependency (slot-ref ws 'dependencies))))

(define-method (add-type! (ws <gw-wrapset>) (name <symbol>) (type <gw-type>))
  (slot-set! ws 'types (acons name type (slot-ref ws 'types)))
  (slot-set! ws 'items (cons type (slot-ref ws 'items))))

(define-method (add-constant! (ws <gw-wrapset>) (constant <gw-constant>))
  (slot-set! ws 'items (cons constant (slot-ref ws 'items))))

(define-method (fold-types kons knil (ws <gw-wrapset>))
  (fold (lambda (pr rest)
          (kons (car pr) (cdr pr) rest))
        knil
        (reverse (slot-ref ws 'types))))

(define-method (for-each-type proc (ws <gw-wrapset>))
  (for-each (lambda (pr)
              (proc (car pr) (cdr pr)))
          (reverse (slot-ref ws 'types))))
  
(define-method (lookup-type (wrapset <gw-wrapset>) (type-name <symbol>))
  (let* ((types-alist (slot-ref wrapset 'types))
         (ret (assq-ref types-alist type-name)))
    (if ret
        ret
        (call-with-current-continuation
         (lambda (exit)
           (for-each
            (lambda (ws)
              (let ((ret (lookup-type ws type-name)))
                (if ret
                    (exit ret))))
            (wrapsets-depended-on wrapset))
           #f)))))

(define-method (typespec (wrapset <gw-wrapset>) (type-sym <symbol>) . options)
  (let ((type (lookup-type wrapset type-sym)))
    (if type
        (make-typespec type options)
        (throw
         'gw:bad-typespec
         (format #f "no type ~S in wrapset ~S" type-sym (name wrapset))))))

(define-method (arguments (wrapset <gw-wrapset>) . argspecs)
  (let loop ((specs argspecs) (args '()))
    (if (null? specs)
        (reverse args)
        (loop (cdr specs) (cons (apply typespec wrapset (car specs)) args)))))

(define-method (add-function! (ws <gw-wrapset>) (function <gw-function>))
  (slot-set! ws 'items (cons function (slot-ref ws 'items))))

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

(define (output-initializer-cgs wrapset lang cgs port)
  (let* ((error-var (gen-c-tmp "error_var"))
         (wrapset-name (name wrapset))
         (wrapset-name-c-sym (any-str->c-sym-str wrapset-name))
         (wrapset-init-func (string-append "gw_init_wrapset_"
                                           wrapset-name-c-sym)))

    (define (output-initializer-cg cg)
      (let ((code (cg lang error-var)))
        (if (not (null? code))
            (render (expand-special-forms code #f '(type range memory misc))
                    port)
            (flatten-display
             (list
              "if ((" error-var ").status != GW_ERR_NONE)"
              "  gw_handle_wrapper_error (&" error-var ",\n"
              "                            \"" wrapset-init-func "\",\n"
              "                            0);\n")
             port))))
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
                                 (wrapset <gw-wrapset>)
                                 (basename <string>))
  (let* ((wrapset-source-name (string-append basename ".c"))
         (wrapset-name-c-sym (any-str->c-sym-str (name wrapset))))
    
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
