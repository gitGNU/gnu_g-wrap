(define-module (g-wrap rti)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  
  #:duplicates last
  
  #:export
  (<gw-rti-wrapset>
   c-info-sym add-type-rti-cg))

(define-class <gw-rti-wrapset> (<gw-wrapset>)
  (c-info-sym #:getter c-info-sym #:init-form (gen-c-tmp "c_info")))

(define-method (add-type-rti-cg (wrapset <gw-rti-wrapset>)
                                (name <symbol>)
                                (type <gw-type>))
  (let ((class-name (class-name type))
        (ws-info (c-info-sym wrapset)))
    (list
     "gw_wrapset_add_type(" ws-info ", \""
     name "\", "
     (if class-name (list "\"" class-name "\"") "NULL") ", "
     "NULL, NULL, NULL, NULL, NULL);\n")))

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

       ;; TODO: subtypes support
       (fold-types
        (lambda (name type code)
          (cons (add-type-rti-cg wrapset name type) code))
        '()
        wrapset)

       "gw_wrapset_register (" ws-info ");\n")))
  
  (next-method)

  (add-cs-global-declarator! wrapset cs-global-declarator)
  (add-cs-declarator! wrapset cs-declarator)
  (add-cs-initializer! wrapset cs-initializer))