(define-module (g-wrap rti)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  
  #:duplicates last
  
  #:export
  (<gw-rti-wrapset>
   c-info-sym add-type-rti-cg
   typespec-cg))

(define-class <gw-rti-wrapset> (<gw-wrapset>)
  (c-info-sym #:getter c-info-sym #:init-form (gen-c-tmp "c_info"))
  (register-functions? #:getter register-functions?
                       #:init-keyword #:register-functions?
                       #:init-value #f))

(define-method (add-type-rti-cg (wrapset <gw-rti-wrapset>)
                                (type <gw-type>))
  (let ((class-name #f) ;; FIXME: (class-name type))
        (ws-info (c-info-sym wrapset)))
    (list
     "gw_wrapset_add_type(" ws-info ", \""
     (name type) "\", "
     (if class-name (list "\"" class-name "\"") "NULL") ", "
     "NULL, NULL, NULL, NULL, NULL);\n")))

(define-method (add-function-rti-cg (wrapset <gw-rti-wrapset>)
                                    (function <gw-function>))
  (let ((arg-types-sym (gen-c-tmp "arg_types"))
        (arg-typespecs-sym (gen-c-tmp "arg_typespecs"))
        (nargs (argument-count function)))
    (list
     "{\n"
     "  const char *" arg-types-sym "[" (number->string nargs) "];\n"
     "  static GWTypeSpec " arg-typespecs-sym "[] = { "
     (map (lambda (arg)
            (list (typespec-cg (type arg) (typespec arg)) ", "))
          (arguments function))
     " };\n"
     (cdr
      (fold (lambda (arg state)
              (let ((idx (car state))
                    (result (cdr state)))
                (cons
                 idx
                 (cons
                 (list
                  "  " arg-types-sym "[" (number->string idx) "] = \""
                  (name (type arg)) "\";\n")
                 result))))
            (cons 0 '())
            (arguments function)))
     "   gw_wrapset_add_function(" (c-info-sym wrapset) ", "
     (c-name function) ", " nargs ", \"" (name (return-type function)) "\", "
     (typespec-cg (return-type function) (return-typespec function)) ", "
     arg-types-sym ", " arg-typespecs-sym ", \"" (name function) "\", "
     (if (generic-name function)
         (list "\"" (symbol->string (generic-name function) "\""))
         "NULL")
     ");\n"
     "}\n")))
  
(define-method (initialize (wrapset <gw-rti-wrapset>) initargs)

  (define (cs-global-declarator lang)
    (list "#include <g-wrap/core-runtime.h>\n"))
  
  (define (cs-declarator lang)
    (list "  GWWrapSet *" (c-info-sym wrapset) " = NULL;\n"))

  (define (cs-initializer lang error-var)
    (let ((ws-info (c-info-sym wrapset)))
      (list
       ws-info " = gw_wrapset_new(\"" (name wrapset) "\", "
       (map (lambda (dep)
              (list "\"" (name dep) "\", "))
            (wrapsets-depended-on wrapset))
       "NULL);\n"

       (fold-functions
        (lambda (func code)
          (cons (add-function-rti-cg wrapset func) code))
        '() wrapset)
       
       ;; TODO: subtypes support
       (fold-types
        (lambda (type code)
          (cons (add-type-rti-cg wrapset type) code))
        '() wrapset)

       "gw_wrapset_register (" ws-info ");\n")))
  
  (next-method)

  (add-cs-global-declarator! wrapset cs-global-declarator)
  (add-cs-declarator! wrapset cs-declarator)
  (add-cs-initializer! wrapset cs-initializer))

(define-method (typespec-cg (type <gw-type>) (typespec <gw-typespec>))
  '("0"))
