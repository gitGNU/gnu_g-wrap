(define-module (g-wrap ffi)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap util)
  
  #:export
  (<gw-ffi-wrapset> uses-ffi?
   <gw-ffi-type>
   ffspec
   wrap-value-function-name unwrap-value-function-name
   destruct-value-function-name 
   wrap-value-function-cg unwrap-value-function-cg destruct-value-function-cg))

(define-class <gw-ffi-wrapset> (<gw-rti-wrapset>)
  (uses-ffi? #:getter uses-ffi? #:init-value #t))

(define-class <gw-ffi-type> (<gw-type>)
  (c-type-name #:getter c-type-name #:init-keyword #:c-type-name)
  (c-const-type-name #:init-keyword #:c-const-type-name)
  (ffspec #:getter ffspec #:init-keyword #:ffspec)
  
  (wrap-value-function-name #:getter wrap-value-function-name)
  (unwrap-value-function-name  #:getter unwrap-value-function-name)
  (destruct-value-function-name #:getter destruct-value-function-name))

(define-method (initialize (type <gw-ffi-type>) initargs)

  (define (gen-name action) (gen-c-tmp-name type action)) ;; Just lazy
  
  (next-method)
  
  (slot-set! type 'wrap-value-function-name (gen-name "wrap_value"))
  (slot-set! type 'unwrap-value-function-name (gen-name "unwrap_value"))
  (slot-set! type 'destruct-value-function-name (gen-name "destruct_value")))


(define-method (add-type-rti-cg (wrapset <gw-ffi-wrapset>)
                                (type <gw-ffi-type>))
  (let (;;(class-name (class-name type))
        (ws-info (c-info-sym wrapset)))
    (list
     "gw_wrapset_add_type(" ws-info ", \""
     (name type) "\", "
     ;;(if class-name (list "\"" class-name "\"") "NULL") ", "
     "NULL, " ;; FIXME
     "&ffi_type_" (ffspec type) ", NULL, "
     (wrap-value-function-name type) ", "
     (unwrap-value-function-name type) ", "
     (destruct-value-function-name type)
     ");\n")))
          
(define-method (pre-call-arg-cg (lang <gw-language>)
                                (type <gw-ffi-type>)
                                (param <gw-value>)
                                status-var)
  (list
   (unwrap-value-cg lang param status-var)
   "if (" `(gw:error? ,status-var type) ")"
   `(gw:error ,status-var arg-type)
   "else if (" `(gw:error? ,status-var range) ")"
   `(gw:error ,status-var arg-range)))


(define-method (call-ccg (lang <gw-language>)
                         (type <gw-ffi-type>)
                         (result <gw-value>)
                         (func-call-code <gw-code>)
                         status-var)
  (list (var result) " = " func-call-code ";\n"))

(define-method (post-call-result-ccg (lang <gw-language>)
                                     (type <gw-ffi-type>)
                                     (result <gw-value>)
                                     status-var)
  (unwrap-value-cg lang result status-var))

(define-generic wrap-value-function-cg)
(define-generic unwrap-value-function-cg)
(define-generic destruct-value-function-cg)

(define-method (global-definitions-cg (lang <gw-language>)
                                      (wrapset <gw-ffi-wrapset>)
                                      (type <gw-ffi-type>))
  (list
   (next-method)
   (wrap-value-function-cg lang type)
   (unwrap-value-function-cg lang type)
   (destruct-value-function-cg lang type)))

(define-method (make-typespec (type <gw-ffi-type>) (options <list>))
  (let ((remainder options))
    (set! remainder (delq 'const remainder))
    (if (and (memq 'caller-owned remainder)
             (memq 'callee-owned remainder))
        (throw 'gw:bad-typespec
               "Bad <gw-ffi-type> options (caller and callee owned!)."
               options))
    (if (not (or (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder)))
        (throw 'gw:bad-typespec
               (format #t "Bad <gw-ffi-type> options for type ~S (must be caller or callee owned!)." type)
               options))
    (set! remainder (delq 'caller-owned remainder))
    (set! remainder (delq 'callee-owned remainder))
    (if (null? remainder)
        (make <gw-typespec> #:type type) ;; FIXME: options
        (throw 'gw:bad-typespec
               "Bad <gw-ffi-type> options - spurious options: " remainder))))
