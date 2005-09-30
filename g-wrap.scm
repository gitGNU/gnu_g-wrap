;;;; File: g-wrap.scm
;;;; Copyright (C) 2004-2005 Andreas Rottmann
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

;;; Commentary:
;;
; This is the core module of G-Wrap, containing language-independent
; code.
;;
;;; Code:

(define-module (g-wrap)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (g-wrap util)
  
  #:export
  (&gw-bad-typespec
   raise-bad-typespec
   raise-stacked
   
   <gw-item>
   description
   all-types-referenced
   
   <gw-constant>
   value typespec 

   <gw-function>
   c-name
   argument-count input-argument-count output-argument-count optional-argument-count
   arguments argument-types argument-typespecs
   input-arguments output-arguments optional-arguments
   return-type return-typespec
   generic-name 
   
   <gw-type>
   needs-result-var?
   wrap-value-cg unwrap-value-cg destroy-value-cg
   pre-call-arg-cg pre-call-result-cg call-arg-cg post-call-result-cg
   post-call-arg-cg call-cg set-value-cg
   
   gen-c-tmp-name
   
   <gw-typespec>
   type options c-type-name all-types add-option!
   make-typespec check-typespec-options parse-typespec-option!
   
   <gw-value>
   var wrapped-var if-typespec-option
   
   <gw-argument>
   visible? default-value output-argument?

   <gw-param>
   number output-param?
   
   <gw-code> <gw-scm-code>
   render no-op? make-scm-code
   has-error-form? expand-special-forms

   <gw-wrapset-class> <gw-wrapset>
   name language wrapsets-depended-on
   fold-types for-each-type lookup-type fold-functions
   consider-types?
   
   add-item! add-type! add-constant! add-function!
   add-client-item!
   
   provide-type-class!
   defines-generic?
   
   wrap-type! wrap-function! wrap-constant!

   get-wrapset generate-wrapset compute-client-types
   ))

;;; Conditions

(define-class &gw-bad-typespec (&error &message)
  (spec #:getter typespec-form #:init-value #f)
  (type #:getter type #:init-value #f)
  (options #:getter typespec-options #:init-value #f))

(define-class &gw-bad-typespec-option (&error &message)
  (option #:getter typespec-option))

(define-class &gw-name-conflict (&error &message)
  (name #:getter conflicting-name)
  (namespace #:getter conflict-namespace))

(define-class &gw-stacked (&message)
  (next #:getter next-condition))

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

(define-method (handle-condition (c &gw-name-conflict))
  (format-error "name conflict: ~A in namespace ~A: ~A"
                (conflicting-name c) (conflict-namespace c)
                (condition-message c)))

;;;

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

(define-method (all-types-referenced (sel <gw-item>))
  '())

;; Upgrade the GOOPS class-name procedure.
(set! class-name (ensure-accessor class-name))

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

;; Here because needs <gw-type>
(define-method (raise-bad-typespec type (options <list>) (msg <string>) . args)
  (raise (condition
          (&gw-bad-typespec
           (type type) (options options)
           (message (apply format #f msg args))))))

(define-method (raise-bad-typespec spec (msg <string>) . args)
  (raise (condition
          (&gw-bad-typespec
           (spec spec)
           (message (apply format #f msg args))))))

(define-method (raise-bad-typespec-option option (msg <string>) . args)
  (raise (condition
          (&gw-bad-typespec-option
           (option option)
           (message (apply format #f msg args))))))

(define-method (raise-stacked (next &condition) (msg <string>) . args)
  (raise (condition
          (&gw-stacked
           (next next)
           (message (apply format #f msg args))))))
  
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
(define-generic destroy-value-cg)

(define-method (destroy-value-cg (type <gw-type>) (value <gw-value>) err)
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
   (destroy-value-cg type result status-var)))


(define-method (post-call-arg-cg (type <gw-type>) (param <gw-value>) err)
  (list
   (if (memq 'out (options (typespec param)))
       (wrap-value-cg type param err)
       '())
  (destroy-value-cg type param err)))

(define-method (set-value-cg (type <gw-type>) (lvalue <gw-value>) rvalue)
  (list (var lvalue) " = " rvalue ";\n"))


;;; Parameters

(define-class <gw-param> (<gw-value>)
  (number #:getter number #:init-keyword #:number))

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
  (slot-push! self 'options option))

(define-method (make-typespec (type <gw-type>) (options <list>))
  (check-typespec-options type options)
  (guard
   (c
    ((is-a? c &gw-bad-typespec-option)
     (raise-bad-typespec type options "bad typespec option ~S: ~A"
                         (typespec-option c)
                         (condition-message c))))
   (let ((typespec (make <gw-typespec> #:type type)))
     (for-each (lambda (opt) (parse-typespec-option! typespec type opt))
               options)
     typespec)))

(define-method (check-typespec-options (type <gw-type>) (options <list>))
  (if (not (null? options))
      (raise-bad-typespec type options
                          "typespec may not have options by default")))

(define-method (parse-typespec-option! (typespec <gw-typespec>)
                                       (type <gw-type>)
                                       (option <symbol>))
  (add-option! typespec option))

(define-method (parse-typespec-option! (typespec <gw-typespec>)
                                       (type <gw-type>)
                                       option)
  (raise-bad-typespec-option option "typespec options must be symbols"))

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

(define-method (input-arguments (func <gw-function>))
  (filter (lambda (arg)
           (and (visible? arg)
                (not (memq 'out (options (typespec arg))))))
         (slot-ref func 'arguments)))

(define-method (input-argument-count (func <gw-function>))
  (count (lambda (arg)
           (and (visible? arg)
                (not (memq 'out (options (typespec arg))))))
         (slot-ref func 'arguments)))

(define-method (output-argument-count (func <gw-function>))
  (count (lambda (arg)
           (memq 'out (options (typespec arg))))
         (slot-ref func 'arguments)))

(define-method (output-arguments (func <gw-function>))
  (filter (lambda (arg)
            (memq 'out (options (typespec arg))))
         (slot-ref func 'arguments)))

(define-method (optional-arguments (func <gw-function>))
  (let loop ((args (reverse (slot-ref func 'arguments))) (opt-args '()))
    (cond ((or (null? args) (and (visible? (car args)) 
                                 (not (default-value (car args)))))
           (reverse opt-args))
          ((and (visible? (car args))
                (not (memq 'out (options (typespec (car args))))))
           (loop (cdr args) (cons (car args) opt-args)))
          (else
           (loop (cdr args) opt-args)))))

;; Returns the number of optional argument (number of consecutive
;; arguments with default values at the end of the argument list).
(define-method (optional-argument-count (func <gw-function>))
  (let loop ((args (reverse (slot-ref func 'arguments))) (count 0))
    (cond ((or (null? args) (and (visible? (car args)) 
                                 (not (default-value (car args)))))
           count)
          ((and (visible? (car args))
                (not (memq 'out (options (typespec (car args))))))
           (loop (cdr args) (+ count 1)))
          (else
           (loop (cdr args) count)))))

(define-method (argument-types (func <gw-function>))
  (map type (slot-ref func 'arguments)))

(define-method (argument-typespecs (func <gw-function>))
  (map typespec (slot-ref func 'arguments)))

(define-method (return-type (func <gw-function>))
  (type (return-typespec func)))

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
  (and (arguments-visible? (type arg)) (not (output-argument? arg))))

(define-method (output-argument? (arg <gw-argument>))
  (memq 'out (options (typespec arg))))

;;; Constants

(define-class <gw-constant> (<gw-item>)
  (name #:getter name #:init-keyword #:name)
  (value #:init-keyword #:value #:getter value)
  (typespec #:init-keyword #:typespec #:getter typespec))

(define-method (type (constant <gw-constant>))
  (type (typespec constant)))

(define-method (all-types-referenced (self <gw-constant>))
  (all-types (typespec self)))

;;;
;;; Code 
;;;
(define-class <gw-code> ())
(define-class <gw-scm-code> (<gw-code>)
  (expression #:getter expression #:init-keyword #:expression))

(define (make-scm-code expression)
  (make <gw-scm-code> #:expression expression))

(define-method (render (code <gw-scm-code>) (port <port>))
  (if (not (null? (expression code)))
      (pretty-print (expression code) port)))

;; default representation: nested string lists
(define-method (render (code <list>) (port <port>))
  (flatten-display code port))

(define-method (render (code <string>) (port <port>))
  (render (list code) port))

(define-method (no-op? (code <list>)) (null? list))


;;;
;;; Wrapsets
;;;

;;; Metaclass - handles wrapset registry
(define-class <gw-wrapset-class> (<class>))

(define-method (initialize (class <gw-wrapset-class>) initargs)
  (next-method)

  (let-keywords
   initargs #t (language id types (dependencies '()))
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

   (class-slot-set-supers-union! class 'dependency-ids dependencies)
   
   (class-slot-set! class 'type-classes (make-hash-table 7))
   (cond
    (types
     (if (not (list? types))
         (error "invalid #:types option (must be list)"))
     (for-each
      (lambda (elt)
        (hashq-set! (class-slot-ref class 'type-classes) (first elt)
                    (second elt)))
      types)))
   ))

(define-class <gw-wrapset> ()
  (name #:getter name #:init-keyword #:name #:allocation #:each-subclass)
  (language #:getter language #:init-keyword #:language
            #:allocation #:each-subclass)
  (type-classes #:allocation #:each-subclass)
  (dependency-ids #:allocation #:each-subclass)
  
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

(define-method (initialize (self <gw-wrapset>) initargs)
  (next-method)
  
  (slot-set! self 'dependencies
             (map (lambda (name)
                    (get-wrapset (language self) name))
                  (slot-ref self 'dependency-ids))))

(define-method (add-item! (self <gw-wrapset>) (item <gw-item>))
  (slot-push! self 'items item))

(define-method (add-client-item! (self <gw-wrapset>) (item <gw-item>))
  (slot-push! self 'client-items item))

(define-method (add-type! (ws <gw-wrapset>) (type <gw-type>))
  (if (hashq-ref (slot-ref ws 'type-hash) (name type))
      (raise (condition
              (&gw-name-conflict
               (name (name type))
               (namespace ws)
               (message (format #f "duplicate type name ~S" type))))))
  (slot-push! ws 'types type)
  (slot-push! ws 'items type)
  (hashq-set! (slot-ref ws 'type-hash) (name type) type))

(define-method (add-function! (ws <gw-wrapset>) (function <gw-function>))
  (slot-push! ws 'items function)
  (slot-push! ws 'functions function)
  (if (generic-name function)
      (let ((handle (hashq-create-handle! (slot-ref ws 'generic-hash)
                                          (generic-name function) '())))
        (set-cdr! handle (cons function (cdr handle))))))

(define-method (add-constant! (ws <gw-wrapset>) (constant <gw-constant>))
  (slot-push! ws 'items constant))

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

(define-method (consider-types? (wrapset <gw-wrapset>) (item <gw-item>))
  #t)

(define (resolve-typespec wrapset spec)
  (let* ((form (cond
                ((symbol? spec) (list spec))
                ((list? spec) spec)
                (else (raise-bad-typespec spec "neither list nor symbol"))))
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
                  (raise-bad-typespec type (cdr form) "bad option ~S" elt))))
              (cdr form)))
        (raise-bad-typespec (car form) (cdr form)
                            "no such type in wrapset `~S'"
                            (name wrapset)))))

(define (resolve-arguments wrapset argspecs)
  (define (argument i spec)
    (if (not (and (list? spec) (>= (length spec) 2)))
        (raise-bad-typespec
         spec
         "argument spec must be a (at least) two-element list"))
    (let ((ts (car spec)))
      (guard
       (c
        (#t (raise-stacked c "while processing argument `~S'" (second spec))))
       (apply
        make <gw-argument>
        #:number i
        #:name (second spec)
        #:typespec (resolve-typespec wrapset ts)
        (fold
         (lambda (opt rest)
           (if (not (and (list? opt) (= (length opt) 2)))
               (raise-bad-typespec spec "invalid argument option ~S" opt))
           (case (first opt)
             ((default) (cons #:default (cons (second opt) rest)))
             (else
              (raise-bad-typespec spec "unknown argument option ~S" opt))))
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
     (#t (raise-stacked c "while processing function `~S'" name)
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

(define-method (all-types-referenced (self <gw-wrapset>))
  (append-map all-types-referenced
              (filter (lambda (item) (consider-types? self item))
                      (slot-ref self 'items))))


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

;; Main entry point for wrapset generation
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
     (lambda (type)
       (let ((type-name (name type)))
         ;;(format #t "considering ~S as client type\n" type)
         (if (not (hashq-ref my-types type-name))
             (hashq-set! client-type-hash type-name type))))
     (all-types-referenced ws))
    (hash-fold (lambda (key val rest) (cons val rest)) '() client-type-hash)))


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


;; Hook in compat layer
(module-use! (resolve-interface '(g-wrap))
             (resolve-interface '(g-wrap compat)))
