(define-module (g-wrap enumeration)
  #:use-module (oop goops)
  
  #:use-module (g-wrap)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap util)

  #:export (<gw-enumeration-type> val-array-name wrap-enum!))

(define-class <gw-enumeration-type> (<gw-simple-rti-type>)
  (values #:init-keyword #:values)
  (val-array-name #:getter val-array-name
                  #:init-form (gen-c-tmp "val_array")))

(define-method (initialize (enum <gw-enumeration-type>) initargs)
  (next-method)

  (slot-set! enum 'ffspec 'sint)) ;; Correct?

(define-method (global-definitions-cg (lang <gw-language>)
                                      (wrapset <gw-wrapset>)
                                      (enum <gw-enumeration-type>))
  (list
   "static GWEnumPair " (slot-ref enum 'val-array-name) "[] = {\n"
   (map
    (lambda (enum-val)
      (let ((c-sym (cdr enum-val))
            (scm-sym (car enum-val)))
        (list
         "  {" c-sym ", \"" scm-sym "\" },\n")))
    (slot-ref enum 'values))
   " { 0, NULL } };\n"))

(define-method (wrap-enum! (ws <gw-wrapset>) . args)
  (add-type! ws (apply make <gw-enumeration-type> args)))
