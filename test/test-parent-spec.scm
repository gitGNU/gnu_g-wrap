(define-module (test test-parent-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-types)

  #:export (<test-parent-wrapset>))

(define-class <test-parent-wrapset> (<gw-wrapset>))
  
(define-method (global-declarations-cg (ws <test-parent-wrapset>))
  (list
   (next-method)
   "#include \"test/g-wrap-test-c-code.h\"\n"))

(define-method (initialize (ws <test-parent-wrapset>) initargs)
  (next-method)
  
  (depends-on! ws 'standard)

  (wrap-as-wct! ws
                #:name '<gw:TestParentObj*>
                #:c-type-name "gwTestParentObj*"
                #:c-const-type-name "const gwTestParentObj*")
  
  (wrap-function! ws
                  #:name 'gw-test-parent-make-obj
                  #:returns '<gw:TestParentObj*>
                  #:c-name "gw_test_parent_make_obj"
                  #:arguments '(((mchars const caller-owned) data))
                  #:description "Make a gwTestParentObj*.")
  
  (wrap-function! ws
                  #:name 'gw-test-parent-same-obj
                  #:returns '<gw:TestParentObj*>
                  #:c-name "gw_test_parent_same_obj"
                  #:arguments '((<gw:TestParentObj*> data))
                  #:description "Make a gwTestParentObj*.")
  
  (wrap-function! ws
                  #:name 'gw-test-parent-display-obj
                  #:returns 'void
                  #:c-name "gw_test_parent_display_obj"
                  #:arguments '(((<gw:TestParentObj*> const) f))))
