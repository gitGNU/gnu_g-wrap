;;;; File: g-wrap.scm
;;;; Copyright (C) 2004 Andreas Rottmann
;;;;
;;;; based upon G-Wrap 1.3.4,
;;;;   Copyright (C) 1996, 1997,1998 Christopher Lee
;;;;   Copyright (C) 1999, 2000, 2001, 2002 Rob Browning
;;;; 
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this software; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, 675 Mass Ave, Cambridge,
;;;; MA 02139, USA.
;;;;

(define-module (g-wrap)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (g-wrap util)
  
  #:export
  (&gw-bad-typespec
   
   <gw-item>
   description
   
   <gw-constant>
   value typespec 

   <gw-function>
   c-name
   argument-count input-argument-count optional-argument-count
   arguments argument-types
   return-type return-typespec
   generic-name 
   
   <gw-type>
   class-name needs-result-var?
   wrap-value-cg unwrap-value-cg destruct-value-cg
   pre-call-arg-cg pre-call-result-cg call-arg-cg post-call-result-cg
   post-call-arg-cg call-cg set-value-cg
   
   global-declarations-cg global-definitions-cg
   declarations-cg initializations-cg init-finalizations-cg
   
   client-global-declarations-cg client-global-definitions-cg
   client-initializations-cg
   
   gen-c-tmp-name
   
   <gw-typespec>
   type options c-type-name all-types add-option!
   make-typespec check-typespec-options parse-typespec-option!
   
   <gw-value>
   var wrapped-var if-typespec-option
   
   <gw-argument>
   visible? default-value

   <gw-param>
   number output-param?
   
   <gw-code>
   render no-op?
   has-error-form? expand-special-forms
   
   <gw-wrapset>
   name language wrapsets-depended-on
   fold-types for-each-type lookup-type fold-functions
   depends-on!

   add-item! add-type! add-constant! add-function!
   add-client-item!
   
   provide-type-class!
   defines-generic?
   
   wrap-type! wrap-function! wrap-constant!

   generate-wrapset
   ))

;;; Conditions

(define-class &gw-bad-typespec (&error &message)
  (spec #:accessor typespec-form #:init-value #f)
  (type #:accessor type #:init-value #f)
  (options #:accessor typespec-options #:init-value #f))

(define-class &gw-stacked (&message)
  (next #:accessor next-condition))

(define-method (format-error msg . args)
  (display "g-wrap: " (current-error-port))
  (apply format (current-error-port) msg args)
  (newline (current-error-port)))

(define-method (handle-condition (c &gw-stacked))
  (format-error "~A:" (condition-message c))
  (handle-condition (next-condition c)))

(define-method (handle-condition (c &gw-bad-typespec))
  (cond
   ((type c)
    (format-error "bad typespec `~A ~A': ~A"
                  (type c) (typespec-options c) (condition-message c)))
   (else
    (format-error "bad typespec `~A': ~A" (typespec-form c)
                  (condition-message c)))))

(define-method (handle-condition (c &gw-bad-element))
  (format-error "bad element ~S in tree ~S" (element c) (tree c)))

;;;

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

(define-class <gw-value> ()
  (typespec #:getter typespec #:init-keyword #:typespec)
  (var #:getter var #:init-keyword #:var)
  (wrapped-var #:getter wrapped-var #:init-keyword #:wrapped-var))

(define-class <gw-param> (<gw-value>)
  (number #:getter number #:init-keyword #:number))

(define-class <gw-type> (<gw-item>)
  (name #:getter name #:init-keyword #:name)
  (class-name #:accessor class-name
              #:init-keyword #:class-name
              #:init-value #f)
  (needs-result-var? #:getter needs-result-var?
                     #:init-keyword #:needs-result-var?
                     #:init-value #t)
  (arguments-visible? #:getter arguments-visible?
                      #:init-keyword #:arguments-visible?
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

;;;
;;; Values
;;;

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

(define-method (destruct-value-cg (type <gw-type>) (value <gw-value>) err)
  '())

(define-method (pre-call-arg-cg (type <gw-type>) (param <gw-value>) err)
  (unwrap-value-cg type param err))

;; What was that for?
;;    "if (" `(gw:error? ,status-var type) ")"
;;    `(gw:error ,status-var arg-type)
;;    "else if (" `(gw:error? ,status-var range) ")"
;;    `(gw:error ,status-var arg-range)))

(define-method (pre-call-result-cg (type <gw-type>) (result <gw-value>) err)
  '())

(define-method (call-arg-cg (type <gw-type>) (value <gw-value>))
  (if (memq 'out (options (typespec value)))
      (list "&" (var value))
      (list (var value))))

(define-method (call-cg (type <gw-type>) (result <gw-value>)
                        func-call-code error-var)
  (list (var result) " = " func-call-code ";\n"))

(define-method (post-call-result-cg (type <gw-type>)
                                    (result <gw-value>)
                                    status-var)
  (list
   (wrap-value-cg type result status-var)
   (destruct-value-cg type result status-var)))


(define-method (post-call-arg-cg (type <gw-type>) (param <gw-param>) err)
  (list
   (if (memq 'out (options (typespec param)))
       (wrap-value-cg type param err)
       '())
  (destruct-value-cg type param err)))

(define-method (set-value-cg (type <gw-type>) (lvalue <gw-value>) rvalue)
  (list (var lvalue) " = " rvalue ";\n"))

;;;

(define-method (visible? (self <gw-param>))
  (>= (number self) 0))

(define-method (output-param? (self <gw-param>))
  (memq 'out (options (typespec self))))

(define-class <gw-typespec> ()
  (type #:init-keyword #:type #:getter type)
  (options #:init-keyword #:options #:getter options #:init-value '()))

(define-method (write (self <gw-typespec>) port)
  (let ((class (class-of self)))
    (display "#<" port)
    (display (class-name class) port)
    (display #\space port)
    (display (name (type self)) port)
    (display #\space port)
    (write (options self) port)
    (display #\> port)))
  
(define-method (all-types (typespec <gw-typespec>))
  (list (type typespec)))

(define-method (add-option! (self <gw-typespec>) (option <symbol>))
  (slot-set! self 'options (cons option (slot-ref self 'options))))

(define-method (make-typespec (type <gw-type>) (options <list>))
  (check-typespec-options type options)
  (let ((typespec (make <gw-typespec> #:type type)))
    (for-each (lambda (opt) (parse-typespec-option! typespec type opt))
              options)
    typespec))

(define-method (check-typespec-options (type <gw-type>) (options <list>))
  (if (not (null? options))
      (raise (condition
              (&gw-bad-typespec
               (type type) (options options)
               (message "typespec may not have options by default"))))))

(define-method (parse-typespec-option! (typespec <gw-typespec>)
                                       (type <gw-type>)
                                       (option <symbol>))
  (add-option! typespec option))

(define-method (parse-typespec-option! (typespec <gw-typespec>)
                                       (type <gw-type>)
                                       option)
  (raise (condition
          (&gw-bad-typespec
           (type type) (options options)
           (message "typespec options must be symbols")))))

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

(define-method (write (self <gw-function>) port)
  (display "#<gw-function " port)
  (display (name self) port)
  (display #\> port))
    
(define-method (return-type (function <gw-function>))
  (type (return-typespec function)))

(define-method (argument-count (func <gw-function>))
  (length (slot-ref func 'arguments)))

(define-method (input-argument-count (func <gw-function>))
  (count (lambda (arg)
           (and (visible? arg)
                (not (memq 'out (options (typespec arg))))))
         (slot-ref func 'arguments)))

;; Returns the number of optional argument (number of consecutive
;; arguments with default values at the end of the argument list).  We
;; count invisible arguments at the argument tail as optional, too.
(define-method (optional-argument-count (func <gw-function>))
  (let loop ((args (reverse (slot-ref func 'arguments))) (count 0))
    (if (or (null? args) (and (visible? (car args)) 
                              (not (default-value (car args)))))
        count
        (loop (cdr args) (+ count 1)))))

(define-method (argument-types (func <gw-function>))
  (map type (slot-ref func 'arguments)))

(define-method (argument-typespecs (func <gw-function>))
  (map typespec (slot-ref func 'arguments)))

(define-method (all-types-referenced (func <gw-function>))
  (fold (lambda (typespec rest)
          (append (all-types typespec) rest))
        '()
        (cons (return-typespec func) (argument-typespecs func))))

;;; Function (formal) arguments

(define-class <gw-argument> ()
  (typespec #:getter typespec #:init-keyword #:typespec)
  (name #:getter name #:init-keyword #:name)
  (default #:getter default-value #:init-keyword #:default #:init-value #f))

(define-method (type (arg <gw-argument>))
  (type (typespec arg)))

(define-method (visible? (arg <gw-argument>))
  (arguments-visible? (type arg)))

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

(define-method (initialize (class <gw-wrapset-class>) initargs)
  (next-method)
  
  (let-keywords
   initargs #t (language id types)
   (if (not language)
       (set! language
             (any (lambda (c) (class-slot-ref c 'language))
                  (filter
                   (lambda (c) (not (eq? <object> c)))
                   (class-direct-supers class)))))
   (class-slot-set! class 'language language)
   
   (if (and language id)
       (register-wrapset-class language id class))

   (if id
       (class-slot-set! class 'name id))

   (class-slot-set! class 'type-classes (make-hash-table 7))
   (if types
       (begin
         (if (not (list? types))
             (error "invalid #:types option (must be list)"))
         (for-each (lambda (elt)
                    (hashq-set! (class-slot-ref class 'type-classes) (first elt)
                                (second elt)))
                   types)
       )
   )))

(define-class <gw-wrapset> ()
  (name #:getter name #:init-keyword #:name #:allocation #:each-subclass)
  (language #:getter language #:init-keyword #:language
            #:allocation #:each-subclass)
  (type-classes #:allocation #:each-subclass)
  
  (dependencies #:getter wrapsets-depended-on #:init-value '())
  (items #:init-value '())
  (client-items #:init-value '())
  (types #:init-value '())
  (type-hash #:init-form (make-hash-table 53))
  (functions #:init-value '())
  
  (generic-hash #:init-form (make-hash-table 31))
  
  (function-class #:init-keyword #:function-class #:init-value <gw-function>)

  #:metaclass <gw-wrapset-class>)

;;; Methods
(define-method (depends-on! (ws <gw-wrapset>) (dep-name <symbol>) . deps)
  (slot-set! ws 'dependencies
             (append!
              (map (lambda (name)
                     (get-wrapset (language ws) name))
                   (cons dep-name deps))
              (slot-ref ws 'dependencies))))

(define-method (add-item! (self <gw-wrapset>) (item <gw-item>))
  (slot-set! self 'items (cons item (slot-ref self 'items))))

(define-method (add-client-item! (self <gw-wrapset>) (item <gw-item>))
  (slot-set! self 'client-items (cons item (slot-ref self 'client-items))))

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
                (else (raise (condition
                              (&gw-bad-typespec
                               (spec spec)
                               (message "neither list nor symbol")))))))
         (type (lookup-type wrapset (car form))))
    (if type
        (make-typespec
         type
         (map (lambda (elt)
                (cond
                 ((list? elt) ; sub-typespec
                  (resolve-typespec wrapset elt))
                 ((symbol? elt)
                  elt)
                 (else
                  (raise (condition
                          (&gw-bad-typespec
                           (type type) (options (cdr form))
                           (message (format #f "bad option ~S" elt))))))))
              (cdr form)))
        (raise (condition
                (&gw-bad-typespec
                 (type (car form)) (options (cdr form))
                 (message (format #f "no such type in wrapset `~S'"
                                  (name wrapset)))))))))

(define (resolve-arguments wrapset argspecs)
  (define (argument i spec)
    (if (not (and (list? spec) (>= (length spec) 2)))
        (raise (condition
                (&gw-bad-typespec
                 (spec spec)
                 (message
                  "argument spec must be a (at least) two-element list")))))
    (let ((ts (car spec)))
      (guard
       (c
        (#t (raise (condition
                    (&gw-stacked
                     (next c)
                     (message (format #f "while processing argument `~S'"
                                      (second spec))))))))
       (apply make <gw-argument>
              #:number i
              #:name (second spec)
              #:typespec (resolve-typespec wrapset ts)
              (fold
               (lambda (opt rest)
                 (if (not (and (list? opt) (= (length opt) 2)))
                     (raise
                      (condition
                       (&gw-bad-typespec
                        (spec spec)
                        (message
                         (format #f "invalid argument option ~S" opt))))))
                 (case (first opt)
                   ((default) (cons #:default (cons (second opt) rest)))
                   (else
                    (raise
                     (condition
                      (&gw-bad-typespec
                       (spec spec)
                       (message
                        (format #f "unknown argument option ~S" opt))))))))
               '() (cddr spec))))))
  
  (let loop ((i 0) (specs argspecs) (args '()))
    (if (null? specs)
        (reverse args)
        (loop (+ i 1) (cdr specs)
              (cons (argument i (car specs)) args)))))

;; High-level interface -- should move low-level stuff to core module
;; and only offer this as API
(define-method (wrap-type! (wrapset <gw-wrapset>) (class-name <symbol>) . args)
  (let ((class (hashq-ref (class-slot-ref
                           (class-of wrapset) 'type-classes) class-name)))
    (if (not class)
        (error "unknown type class ~S" class-name)) ;; FIXME: better handling
    (add-type! wrapset (apply make class args))))

(define-method (wrap-function! (wrapset <gw-wrapset>) . args)
  ;;(format #t "wrapping ~S\n" args)
  (let-keywords
   args #f (name returns c-name arguments description generic-name)
   (guard
    (c
     (#t (raise
          (condition
           (&gw-stacked
            (next c)
            (message (format #f "while processing function `~S'" name)))))
         ;; TODO: Find a way how go on and exit with failure at the end
         (exit 1)))
    
    (add-function!
     wrapset (make (slot-ref wrapset 'function-class)
               #:name name
               #:returns (resolve-typespec wrapset returns)
               #:c-name c-name
               #:arguments (resolve-arguments wrapset arguments)
               #:description description
               #:generic-name generic-name)))))

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

(define *wrapset-registry* (make-hash-table 7))

(define-method (register-wrapset-class (lang <symbol>) (name <symbol>)
                                       (class <class>))
  (let ((key (cons lang name)))
    (if (hash-ref *wrapset-registry* key)
        (error "tried to re-register wrapset class" lang name class *wrapset-registry*))
    (hash-set! *wrapset-registry* key (cons class #f))))

(define-method (get-wrapset (lang <symbol>) (name <symbol>))
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
(define (output-initializer-cgs wrapset cgs port)
  (let* ((error-var (gen-c-tmp "error_var"))
         (wrapset-name (name wrapset))
         (wrapset-name-c-sym (any-str->c-sym-str
                              (symbol->string wrapset-name)))
         (wrapset-init-func (string-append "gw_init_wrapset_"
                                           wrapset-name-c-sym)))

    (define (output-initializer-cg cg)
      (let ((code (cg error-var)))
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

(define-method (generate-wrapset (lang <symbol>)
                                 (name <symbol>)
                                 (basename <string>))
  (let ((had-error? #f))
    (guard
     (c
      (#t (handle-condition c)
          (set! had-error? #t)))
     (generate-wrapset lang (get-wrapset lang name) basename))
    (if had-error?
        (exit 1))))

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
        (all-types-referenced func)))
     (slot-ref ws 'functions))
    (hash-fold (lambda (key val rest) (cons val rest)) '() client-type-hash)))

(define (generate-wrapset-cs wrapset port)
  (define (dsp-list lst)
    (for-each (lambda (s) (display s port)) lst))

  (define (render-client cg)
      (for-each (lambda (ws)
                  (render (cg ws) port))
                (wrapsets-depended-on wrapset)))
  
  (define (render-client-items cg)
      (for-each (lambda (ws)
                  (for-each (lambda (item)
                              (render (cg ws item) port))
                            (reverse (slot-ref ws 'client-items))))
              (wrapsets-depended-on wrapset)))
  
  (let ((wrapset-name-c-sym (any-str->c-sym-str
                             (symbol->string (name wrapset))))
        (client-types (compute-client-types wrapset))
        (items (reverse (slot-ref wrapset 'items))))

    (define (render-items cg)
      (for-each (lambda (item)
                  (render (cg wrapset item) port))
                items))
    
    (define (render-client-types cg)
      (for-each (lambda (type)
                  (render (cg wrapset type) port))
                client-types))
    
    ;;(format #t "client types: ~S\n" client-types)
    (dsp-list
     (list
      "/* Generated by G-Wrap-TNG: an experimental wrapper engine */\n"
      "\n"))
    
    (render-items before-includes-cg)
    (render-client-items before-includes-cg)
    
    (render (global-declarations-cg wrapset) port)
    (render-items global-declarations-cg)

    (render-client-items global-declarations-cg)
    (render-client client-global-declarations-cg)
    (render-client-types client-global-declarations-cg)

    (dsp-list
     (list "void gw_init_wrapset_" wrapset-name-c-sym "(GWLangArena);\n"))
    
    (render (global-definitions-cg wrapset) port)
    (render-items global-definitions-cg)

    (render-client-items global-definitions-cg)
    (render-client client-global-definitions-cg)
    (render-client-types client-global-definitions-cg)
    
    ;; The initialization function
    (dsp-list
     (list
      "void\n"
      "gw_init_wrapset_" wrapset-name-c-sym "(GWLangArena gw__arena) {\n"
      "  static int gw_wrapset_initialized = 0;\n"
      "\n"))

    (render (declarations-cg wrapset) port)
    (render-items declarations-cg)
    
    (dsp-list
     (list
      "  if(gw_wrapset_initialized)\n"
      "   return;\n"
      "\n"))

    ;; TODO: deobfuscate
    (output-initializer-cgs
     wrapset
     (append!
      (list (lambda (error-var)
              (initializations-cg wrapset error-var)))
      (map (lambda (item)
             (lambda (error-var)
               (client-initializations-cg wrapset item error-var)))
           client-types)
      (map (lambda (item)
             (lambda (error-var)
               (initializations-cg wrapset item error-var)))
           (let-values (((types others) (partition! (lambda (item)
                                                      (is-a? item <gw-type>))
                                                    items)))
             (append! types others)))
      (list (lambda (error-var)
              (init-finalizations-cg wrapset error-var)))
      (map (lambda (item)
             (lambda (error-var)
               (init-finalizations-cg wrapset item error-var)))
           items))
     port)

    
    (dsp-list
     (list
      "    gw_wrapset_initialized = 1;\n"
      "}\n"))))


(define-method (generate-wrapset (lang <symbol>)
                                 (wrapset <gw-wrapset>)
                                 (basename <string>))
  (let ((wrapset-source-name (string-append basename ".c"))
        (client-types (compute-client-types wrapset)))

    (call-with-output-file/cleanup
     wrapset-source-name
     (lambda (port)
       (generate-wrapset-cs wrapset port)))))

;;; Default implementations (no-ops)

(define-method (before-includes-cg (wrapset <gw-wrapset>)
                                   (item <gw-item>))
  '())

(define-method (global-declarations-cg (wrapset <gw-wrapset>))
  '())

(define-method (global-declarations-cg (wrapset <gw-wrapset>)
                                       (item <gw-item>))
  '())

(define-method (client-global-declarations-cg (wrapset <gw-wrapset>))
  '())

(define-method (client-global-declarations-cg (wrapset <gw-wrapset>)
                                              (item <gw-item>))
  '())

(define-method (global-definitions-cg (wrapset <gw-wrapset>))
  '())

(define-method (global-definitions-cg (wrapset <gw-wrapset>)
                                      (item <gw-item>))
  '())

(define-method (client-global-definitions-cg (wrapset <gw-wrapset>))
  '())

(define-method (client-global-definitions-cg (wrapset <gw-wrapset>)
                                             (item <gw-item>))
  '())

(define-method (declarations-cg (wrapset <gw-wrapset>))
  '())

(define-method (declarations-cg (wrapset <gw-wrapset>) (item <gw-item>))
  '())

(define-method (initializations-cg (wrapset <gw-wrapset>) error-var)
  '())

(define-method (initializations-cg (wrapset <gw-wrapset>)
                                   (item <gw-item>)
                                   error-var)
  '())

(define-method (client-initializations-cg (wrapset <gw-wrapset>)
                                          (item <gw-item>)
                                          error-var)
  '())

(define-method (init-finalizations-cg (wrapset <gw-wrapset>) err)
  '())

(define-method (init-finalizations-cg (wrapset <gw-wrapset>)
                                      (item <gw-item>) err)
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

(define (expand-gw-error args param allowed-errors top-form labels)
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
         (goto-cg labels (format #f "post_call_arg_~A" (number param)))
         "")
     
     "}\n")))

(define (has-error-form? tree)
  (if (and (list? tree) (not (null? tree)))
      (if (eq? (car tree) 'gw:error)
          #t
          (any has-error-form? tree))
      #f))
   
(define* (expand-special-forms tree param allowed-errors #:key labels)
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
         (expand-gw-error (cdr tree) param allowed-errors top-form labels))
        (else
         (map
          (lambda (elt) (expand-helper elt param allowed-errors top-form))
          tree))))
     (else tree)))
  (expand-helper tree param allowed-errors tree))


