
;; Don't need this, no one else will be loading this file.
;(define-module (g-wrap gw-test-child-spec)
;  :use-module (g-wrap))

(use-modules (g-wrap gw-wct-spec))
(use-modules (g-wrap gw-standard-spec))

(let ((m (gw:new-wrapset "gw-test-child")))

  (gw:wrapset-set-guile-module! m '(test gw-test-child))

  (gw:wrapset-depends-on m "gw-standard")

  (gw:wrapset-depends-on m "gw-wct")

  (gw:wrapset-depends-on m "gw-test-parent")

  (gw:wrapset-add-cs-declarations!
   m
   (lambda (wrapset client-wrapset)
     (if client-wrapset
         '()
         "#include \"g-wrap-test-c-code.h\"\n")))

  (let ((nnt (gw:wrap-as-wct
              m
              '<gw:TestChildObj*>
              "gwTestChildObj*" "const gwTestChildObj*")))
    #t)

  (gw:wrap-function
   m
   'gw-test-child-make-obj
   '<gw:TestChildObj*>
   "gw_test_child_make_obj"
   '(((<gw:mchars> const caller-owned) data))
   "Make a gwTestChildObj*.")

  (gw:wrap-function
   m
   'gw-test-child-same-obj
   '<gw:TestChildObj*>
   "gw_test_child_same_obj"
   '((<gw:TestChildObj*> data))
   "Make a gwTestChildObj*.")

  (gw:wrap-function
   m
   'gw-test-child-display-obj
   '<gw:void>
   "gw_test_child_display_obj"
   '(((<gw:TestChildObj*> const) f)))

  (gw:wrap-function
   m
   'gw-test-child-pass-back-parent-obj
   '<gw:TestParentObj*>
   "gw_test_child_pass_back_parent_obj"
   '((<gw:TestParentObj*> f)))
  
  #t)
