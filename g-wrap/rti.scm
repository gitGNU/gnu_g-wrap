;;;; File: rti.scm
;;;; Copyright (C) 2004 Andreas Rottmann
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

(define-module (g-wrap rti)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  
  #:export
  (<gw-rti-wrapset>
   c-info-sym typespec-cg
   function-rti? uses-rti-for-function?
   
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

(define-class <gw-rti-type-class> (<class>))

(define-method (initialize (class <gw-rti-type-class>) initargs)
  (next-method)
  ;; Inherit the allowed options
  (let-keywords
   initargs #t ((allowed-options '()))
   (class-slot-set! class 'allowed-options
                    (apply append
                           (cons
                            allowed-options
                            (map (lambda (c)
                                   (class-slot-ref c 'allowed-options))
                                 (filter
                                  (lambda (c) (not (eq? <gw-type> c)))
                                  (class-direct-supers class))))))))

(define-class <gw-rti-type> (<gw-type>)
  (allowed-options #:init-value '() #:allocation #:each-subclass)
  (c-type-name #:getter c-type-name #:init-keyword #:c-type-name)
  (c-const-type-name #:init-keyword #:c-const-type-name)
  (ffspec #:getter ffspec #:init-keyword #:ffspec)
  
  (wrap-value-function-name #:getter wrap-value-function-name)
  (unwrap-value-function-name  #:getter unwrap-value-function-name)
  (destruct-value-function-name #:getter destruct-value-function-name)
  
  #:metaclass <gw-rti-type-class>)

(define-method (c-type-name (type <gw-rti-type>) (typespec <gw-typespec>))
  (slot-ref type (if (memq 'const (options typespec))
                     'c-const-type-name
                     'c-type-name)))

(define-method (initialize (type <gw-rti-type>) initargs)

  (define (gen-name action) (gen-c-tmp-name type action)) ;; Just lazy
  
  (next-method)

  (if (not (slot-bound? type 'c-const-type-name))
      (slot-set! type 'c-const-type-name (string-append "const "
                                                        (c-type-name  type))))
  
  (slot-set! type 'wrap-value-function-name (gen-name "wrap_value"))
  (slot-set! type 'unwrap-value-function-name (gen-name "unwrap_value"))
  (slot-set! type 'destruct-value-function-name (gen-name "destruct_value")))

(define-generic wrap-value-function-cg)
(define-generic unwrap-value-function-cg)
(define-generic destruct-value-function-cg)

(define-method (global-definitions-cg (wrapset <gw-rti-wrapset>)
                                      (type <gw-rti-type>))
  (list
   (next-method)
   (wrap-value-function-cg type)
   (unwrap-value-function-cg type)
   (destruct-value-function-cg type)))

(define-method (check-typespec-options (type <gw-rti-type>) (options <list>))
  (let ((remainder options))
    (set! remainder (delq 'const remainder))
    (set! remainder (delq 'out remainder))
    (if (and (memq 'caller-owned remainder)
             (memq 'callee-owned remainder))
        (raise (condition
                (&gw-bad-typespec
                 (type type) (options options)
                 (message "bothe caller and callee owned")))))
    (if (not (or (memq 'caller-owned remainder)
                 (memq 'callee-owned remainder)))
        (raise (condition
                (&gw-bad-typespec
                 (type type) (options options)
                 (message "must be caller or callee owned" type)))))
    (set! remainder (delq 'caller-owned remainder))
    (set! remainder (delq 'callee-owned remainder))
    (for-each (lambda (opt) (set! remainder (delq opt remainder)))
              (slot-ref type 'allowed-options))
    (if (not (null? remainder))
        (raise (condition
                (&gw-bad-typespec
                 (type type) (options options)
                 (message (format #f "spurious options ~S" remainder))))))))

(define-class <gw-simple-rti-type> (<gw-rti-type>))

(define-method (make-typespec (type <gw-simple-rti-type>) (options <list>))
  (let ((typespec (next-method)))
    (add-option! typespec 'caller-owned)
    typespec))

(define-method (check-typespec-options (type <gw-simple-rti-type>)
                                       (options <list>))
  (let ((remainder options))
    (set! remainder (delq 'out remainder))
    (if (not (null? remainder))
        (raise (condition
                (&gw-bad-typespec
                 (type type) (options options)
                 (message (format #f "spurious options ~S" options))))))))

(define-method (initializations-cg (wrapset <gw-rti-wrapset>)
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
     (c-name function) ", " nargs ", 0, \"" (name (return-type function)) "\", "
     (typespec-cg (return-type function) (return-typespec function)) ", "
     arg-types ", " arg-typespecs ", \"" (name function) "\", "
     (if (generic-name function)
         (list "\"" (symbol->string (generic-name function)) "\"")
         "NULL")
     ");\n"
     "}\n")))

;; Returns #t if we can support RTI for the function and it is enabled
(define-method (uses-rti-for-function? (wrapset <gw-rti-wrapset>)
                                       (function <gw-function>))
  (and (slot-ref wrapset 'function-rti?)
       (every (lambda (type) (is-a? type <gw-rti-type>))
              (cons (return-type function)
                    (argument-types function)))
       (every (lambda (arg) (not (or (default-value arg)
                                     (output-argument? arg))))
              (arguments function))))

(define-method (initializations-cg (wrapset <gw-rti-wrapset>)
                                   (function <gw-function>)
                                   error-var)
  (list
   (if (uses-rti-for-function? wrapset function)
       (add-function-rti-cg wrapset function)
       (next-method))))

(define-method (global-declarations-cg (wrapset <gw-rti-wrapset>))
  (list (next-method)
        "#include <g-wrap/core-runtime.h>\n"))

(define-method (declarations-cg (wrapset <gw-rti-wrapset>))
  (list (next-method)
        "  GWWrapSet *" (c-info-sym wrapset) " = NULL;\n"))

(define-method (initializations-cg (wrapset <gw-rti-wrapset>) err)
  (list
   (next-method)
   (c-info-sym wrapset) " = gw_wrapset_new(gw__arena, \"" (name wrapset) "\", "
   (map (lambda (dep)
          (list "\"" (name dep) "\", "))
        (wrapsets-depended-on wrapset))
   "NULL);\n"))
    
(define-method (init-finalizations-cg (wrapset <gw-rti-wrapset>) err)
  (list (next-method)
        "gw_wrapset_register (" (c-info-sym wrapset) ");\n"))

(define-method (typespec-cg (type <gw-type>) (typespec <gw-typespec>))
  '("0"))

(define-method (typespec-cg (type <gw-rti-type>) (typespec <gw-typespec>))
  (let ((options (options typespec)))
    (list
     (cond ((memq 'caller-owned options) "GW_TYPESPEC_CALLER_OWNED")
           ((memq 'callee-owned options) "GW_TYPESPEC_CALLEE_OWNED")
           (else (error "bogus typespec options" type options))))))
