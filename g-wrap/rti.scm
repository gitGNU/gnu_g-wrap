(define-module (g-wrap rti)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  
  #:export
  (<gw-rti-wrapset>
   function-rti? c-info-sym typespec-cg

   <gw-rti-value>
   
   <gw-rti-type>
   ffspec
   wrap-value-function-name unwrap-value-function-name
   destruct-value-function-name 
   wrap-value-function-cg unwrap-value-function-cg destruct-value-function-cg
   
   <gw-simple-rti-type>))

(define-class <gw-rti-wrapset> (<gw-wrapset>)
  (c-info-sym #:getter c-info-sym #:init-form (gen-c-tmp "c_info"))
  (function-rti? #:getter function-rti?
                 #:init-keyword #:function-rti?
                 #:init-value #t))

(define-class <gw-rti-value> (<gw-value>))

(define-method (if-typespec-option (value <gw-rti-value>) (option <symbol>)
                                   code1 . code2-opt)
  (let ((code2 (cond ((null? code2-opt) #f)
                     ((and (list? code2-opt) (= (length code2-opt) 1))
                      (car code2-opt))
                     (else (error "bogus parameters")))))
  (list
   "if (" (typespec value) " & GW_TYPESPEC_"
   (string-upcase (any-str->c-sym-str (symbol->string option)))
   ") {\n"
   code1
   "}\n"
   (if code2
       (list "else {\n" code2 "}\n")
       '()))))

;; FIXME: This allowed-typespec stuff is kind of ugly - subclass
;; typespecs, too?

(define-class <gw-rti-type> (<gw-type>)
  (allowed-options #:init-keyword #:options #:init-value '()
                   #:allocation #:each-subclass)
  (c-type-name #:getter c-type-name #:init-keyword #:c-type-name)
  (c-const-type-name #:init-keyword #:c-const-type-name)
  (ffspec #:getter ffspec #:init-keyword #:ffspec)
  
  (wrap-value-function-name #:getter wrap-value-function-name)
  (unwrap-value-function-name  #:getter unwrap-value-function-name)
  (destruct-value-function-name #:getter destruct-value-function-name))

(define-method (c-type-name (type <gw-rti-type>) (typespec <gw-typespec>))
  (slot-ref type (if (memq 'const (options typespec))
                     'c-const-type-name
                     'c-type-name)))

(define-method (initialize (type <gw-rti-type>) initargs)

  (define (gen-name action) (gen-c-tmp-name type action)) ;; Just lazy
  
  (next-method)
  
  (slot-set! type 'wrap-value-function-name (gen-name "wrap_value"))
  (slot-set! type 'unwrap-value-function-name (gen-name "unwrap_value"))
  (slot-set! type 'destruct-value-function-name (gen-name "destruct_value")))

(define-method (pre-call-arg-cg (lang <gw-language>)
                                (type <gw-rti-type>)
                                (param <gw-value>)
                                status-var)
  (list
   (unwrap-value-cg lang type param status-var)
   "if (" `(gw:error? ,status-var type) ")"
   `(gw:error ,status-var arg-type)
   "else if (" `(gw:error? ,status-var range) ")"
   `(gw:error ,status-var arg-range)))


(define-method (call-ccg (lang <gw-language>)
                         (type <gw-rti-type>)
                         (result <gw-value>)
                         (func-call-code <gw-code>)
                         status-var)
  (list (var result) " = " func-call-code ";\n"))

(define-method (post-call-result-cg (lang <gw-language>)
                                    (type <gw-rti-type>)
                                    (result <gw-value>)
                                    status-var)
  (wrap-value-cg lang type result status-var))

(define-generic wrap-value-function-cg)
(define-generic unwrap-value-function-cg)
(define-generic destruct-value-function-cg)

(define-method (global-definitions-cg (lang <gw-language>)
                                      (wrapset <gw-rti-wrapset>)
                                      (type <gw-rti-type>))
  (list
   (next-method)
   (wrap-value-function-cg lang type)
   (unwrap-value-function-cg lang type)
   (destruct-value-function-cg lang type)))

(define-method (make-typespec (type <gw-rti-type>) (options <list>))
  (let ((remainder options))
    (set! remainder (delq 'const remainder))
    (if (and (memq 'caller-owned remainder)
             (memq 'callee-owned remainder))
        (throw 'gw:bad-typespec
               (format #f "Bad rti options for ~S (caller and callee owned!)." type)
               options))
    (if (not (or (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder)))
        (throw 'gw:bad-typespec
               (format #f "Bad options for ~S (must be caller or callee owned!)." type)
               options))
    (set! remainder (delq 'caller-owned remainder))
    (set! remainder (delq 'callee-owned remainder))
    (for-each (lambda (opt) (set! remainder (delq! opt remainder)))
              (slot-ref type 'allowed-options))
    (if (null? remainder)
        (make <gw-typespec> #:type type #:options options)
        (throw 'gw:bad-typespec
               (format #f "Bad options for ~S - spurious options ~S." type remainder)))))

(define-class <gw-simple-rti-type> (<gw-rti-type>))

(define-method (make-typespec (type <gw-simple-rti-type>) (options <list>))
  (if (null? options)
      (make <gw-typespec> #:type type #:options '(caller-owned))
      (throw 'gw:bad-typespec
             (format #f "Bad options for ~S - spurious options ~S."
                     type options))))


(define-method (initializations-cg (lang <gw-language>)
                                   (wrapset <gw-rti-wrapset>)
                                   (type <gw-rti-type>)
                                   status-var)
  (let ((class-name (class-name type))
        (ws-info (c-info-sym wrapset)))
    (list
     (next-method)
     
     "gw_wrapset_add_type(" ws-info ", \""
     (name type) "\", "
     (if class-name (list "\"" class-name "\"") "NULL") ", "
     "&ffi_type_" (ffspec type) ", NULL, "
     (wrap-value-function-name type) ", "
     (unwrap-value-function-name type) ", "
     (destruct-value-function-name type)
     ");\n")))

(define (add-function-rti-cg wrapset function)
  (let* ((nargs (argument-count function))
         (arg-types (if (> nargs 0) (gen-c-tmp "arg_types") "NULL"))
         (arg-typespecs (if (> nargs 0) (gen-c-tmp "arg_typespecs") "NULL")))
    (list
     "{\n"
     (if (= nargs 0)
         '()
         (list 
          "  const char *" arg-types "[" (number->string nargs) "];\n"
          "  static GWTypeSpec " arg-typespecs "[] = { "
          (map (lambda (arg)
                   (list (typespec-cg (type arg) (typespec arg)) ", "))
               (arguments function))
          " };\n"
          (cdr
             (fold (lambda (arg state)
                     (let ((idx (car state))
                           (result (cdr state)))
                       (cons
                        (+ idx 1)
                        (cons
                         (list
                          "  " arg-types "[" (number->string idx) "] = \""
                          (name (type arg)) "\";\n")
                         result))))
                   (cons 0 '())
                   (arguments function)))))
     "   gw_wrapset_add_function(" (c-info-sym wrapset) ", "
       (c-name function) ", " nargs ", \"" (name (return-type function)) "\", "
       (typespec-cg (return-type function) (return-typespec function)) ", "
       arg-types ", " arg-typespecs ", \"" (name function) "\", "
       (if (generic-name function)
           (list "\"" (symbol->string (generic-name function)) "\"")
           "NULL")
       ");\n"
       "}\n")))

(define (use-rti-for-function? wrapset function)
  (and (slot-ref wrapset 'function-rti?)
       (every (lambda (type)
                (is-a? type <gw-rti-type>))
              (cons (return-type function)
                    (map type (arguments function))))))

(define-method (global-declarations-cg (lang <gw-language>)
                                       (wrapset <gw-rti-wrapset>)
                                       (function <gw-function>))
  (list
   (if (use-rti-for-function? wrapset function)
       '()
       (next-method))))

(define-method (initializations-cg (lang <gw-language>)
                                   (wrapset <gw-rti-wrapset>)
                                   (function <gw-function>)
                                   error-var)
  (list
   (if (use-rti-for-function? wrapset function)
       (add-function-rti-cg wrapset function)
       (next-method))))

(define-method (initialize (wrapset <gw-rti-wrapset>) initargs)
  (next-method)
    
  (let ((ws-info (c-info-sym wrapset)))
    
    (define (cs-global-declarator lang)
      (list "#include <g-wrap/core-runtime.h>\n"))
  
    (define (cs-initializer lang error-var)
      (list
       ws-info " = gw_wrapset_new(gw__arena, \"" (name wrapset) "\", "
       (map (lambda (dep)
              (list "\"" (name dep) "\", "))
            (wrapsets-depended-on wrapset))
       "NULL);\n"))
    
    (add-cs-global-declarator! wrapset cs-global-declarator)
    
    (add-cs-declarator!
     wrapset
     (lambda (lang)
       (list "  GWWrapSet *" (c-info-sym wrapset) " = NULL;\n")))
  
    (add-cs-initializer! wrapset cs-initializer)
    (add-cs-init-finalizer! wrapset
                            (lambda (lang error-var)
                              (list "gw_wrapset_register (" ws-info ");\n")))))
  

(define-method (typespec-cg (type <gw-type>) (typespec <gw-typespec>))
  '("0"))

(define-method (typespec-cg (type <gw-rti-type>) (typespec <gw-typespec>))
  (let ((options (options typespec)))
    (list
     (cond ((memq 'caller-owned options) "GW_TYPESPEC_CALLER_OWNED")
           ((memq 'callee-owned options) "GW_TYPESPEC_CALLEE_OWNED")
           (else (error "bogus typespec options" type options))))))
