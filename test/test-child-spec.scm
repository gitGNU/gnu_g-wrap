(define-module (test test-child-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-types)

  #:export (<test-child-wrapset>))

(define-class <test-child-wrapset> (<gw-wrapset>))
  
(define-method (global-declarations-cg (ws <test-child-wrapset>))
  (list
   (next-method)
   "#include \"test/g-wrap-test-c-code.h\"\n"))

(define-method (initialize (ws <test-child-wrapset>) initargs)
  (next-method)
  
  (depends-on! ws 'test-parent)
  
  (wrap-as-wct! ws
                #:name '<gw:TestChildObj*>
                #:c-type-name "gwTestChildObj*"
                #:c-const-type-name "const gwTestChildObj*")
  
  (wrap-function! ws
                  #:name 'gw-test-child-make-obj
                  #:returns '<gw:TestChildObj*>
                  #:c-name "gw_test_child_make_obj"
                  #:arguments '(((mchars const caller-owned) data))
                  #:description "Make a gwTestChildObj*.")

  (wrap-function! ws
                  #:name 'gw-test-child-same-obj
                  #:returns '<gw:TestChildObj*>
                  #:c-name "gw_test_child_same_obj"
                  #:arguments '((<gw:TestChildObj*> data))
                  #:description "Make a gwTestChildObj*.")

  (wrap-function! ws
                  #:name 'gw-test-child-display-obj
                  #:returns 'void
                  #:c-name "gw_test_child_display_obj"
                  #:arguments '(((<gw:TestChildObj*> const) f)))

  (wrap-function! ws
                  #:name 'gw-test-child-pass-back-parent-obj
                  #:returns '<gw:TestParentObj*>
                  #:c-name "gw_test_child_pass_back_parent_obj"
                  #:arguments '((<gw:TestParentObj*> f))))
