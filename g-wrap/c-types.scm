;; Standard C types

(define-module (g-wrap c-types)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  #:use-module (g-wrap rti)

  #:export (wrap-simple-type!
  
            <gw-ranged-integer-type>
            wrap-ranged-integer-type!
            
            <gw-ctype-void> <gw-ctype-mchars>
            
            <gw-wct>
            wrap-as-wct!))

(define-generic wrap-simple-type!)

;;;
;;; Ranged integers
;;;

(define-class <gw-ranged-integer-type> (<gw-simple-rti-type>)
  (min #:init-keyword #:min #:init-value #f)
  (max #:init-keyword #:max)
  min-var
  max-var)

(define-generic wrap-ranged-integer-type!)

(define-method (initialize (type <gw-ranged-integer-type>) initargs)
  (next-method)
  (let ((c-sym-name (any-str->c-sym-str (c-type-name type))))
    (slot-set! type 'min-var
               (gen-c-tmp (string-append "range_minval" c-sym-name)))
    (slot-set! type 'max-var
               (gen-c-tmp (string-append "range_minval" c-sym-name)))))

 ;; void is class of its, own, of course ;-)
(define-class <gw-ctype-void> (<gw-simple-rti-type>))

(define-method (unwrap-value-cg (lang <gw-language>)
                                (type <gw-ctype-void>)
                                (value <gw-value>) error-var)
  '()) 

(define-method (pre-call-arg-cg (lang <gw-language>)
                                (type <gw-ctype-void>)
                                (param <gw-value>)
                                status-var)
  (error "Can't use void as an argument type."))

(define-method (post-call-arg-cg (lang <gw-language>)
                                 (type <gw-ctype-void>)
                                 (param <gw-value>)
                                 status-var)
  (error "Can't use void as an argument type."))

;; no result assignment.
(define-method (call-ccg (lang <gw-language>)
                         (type <gw-ctype-void>)
                         (result <gw-value>)
                         (func-call-code <gw-code>)
                         status-var)
  (list func-call-code ";\n"))

(define-class <gw-ctype-mchars> (<gw-rti-type>))

(define-method (destruct-value-cg (lang <gw-language>)
                                  (type <gw-ctype-mchars>)
                                  (value <gw-value>)
                                  error-var)
  (let ((c-var (var value)))
    (if-typespec-option value 'caller-owned
                        (list "if (" c-var ") free (" c-var ");\n"))))

;;;
;;; Wrapped C Types
;;;

(define-class <gw-wct> (<gw-rti-type>))

(define-method (initialize (wct <gw-wct>) initargs)
  (next-method)
  (slot-set! wct 'ffspec 'pointer))

(define-method (make-typespec (type <gw-wct>) (options <list>))
  (let ((remainder options))
    (set! remainder (delq 'const options))
    (if (null? remainder)
        (make <gw-typespec> #:type type #:options '(caller-owned))
        (throw 'gw:bad-typespec
               "Bad <gw-wct> options - spurious options: " remainder))))

(define-method (wrap-as-wct! (wrapset <gw-wrapset>) . args)
  (let ((wct (apply make <gw-wct> args)))
    (add-type! wrapset wct)
    wct))
