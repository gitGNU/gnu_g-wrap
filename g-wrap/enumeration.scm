(define-module (g-wrap enumeration)
  #:use-module (oop goops)
  
  #:use-module (g-wrap)
  #:use-module (g-wrap ffi)
  #:use-module (g-wrap util)

  #:export (<gw-enumeration-type>))

(define-class <gw-enumeration-type> (<gw-ffi-type>)
  (values #:init-keyword #:values)
  (val-array-name #:getter val-array-name
                  #:init-form (gen-c-tmp "val_array")))

(define-method (global-definitions-cg (lang <gw-language>)
                                      (wrapset <gw-wrapset>)
                                      (enum <gw-enumeration-type>))
  (list
   "static GWEnumPair " (slot-ref enum 'val-array-name) "[] = {\n"
   (map
    (lambda (enum-val)
      (let ((c-sym (car enum-val))
            (scm-sym (cdr enum-val)))
        (list
         "  {" c-sym ", \"" scm-sym "\" },\n")))
    (slot-ref enum 'values))
   " { 0, NULL } };\n"))
