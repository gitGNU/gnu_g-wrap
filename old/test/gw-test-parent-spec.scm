
;; Don't need this, no one else will be loading this file.
;(define-module (g-wrap gw-test-spec)
;  :use-module (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))

(let ((m (gw:new-wrapset "gw-test-parent")))

  (gw:wrapset-set-guile-module! m '(test gw-test-parent))

  (gw:wrapset-depends-on m "gw-standard")

  (gw:wrapset-depends-on m "gw-wct")

  (gw:wrapset-add-cs-declarations!
   m
   (lambda (wrapset client-wrapset)
     (if client-wrapset
         '()
         "#include \"g-wrap-test-c-code.h\"\n")))

  (let ((nnt (gw:wrap-as-wct
              m
              '<gw:TestParentObj*>
              "gwTestParentObj*" "const gwTestParentObj*")))
    #t)

  (gw:wrap-function
   m
   'gw-test-parent-make-obj
   '<gw:TestParentObj*>
   "gw_test_parent_make_obj"
   '(((<gw:mchars> const caller-owned) data))
   "Make a gwTestParentObj*.")

  (gw:wrap-function
   m
   'gw-test-parent-same-obj
   '<gw:TestParentObj*>
   "gw_test_parent_same_obj"
   '((<gw:TestParentObj*> data))
   "Make a gwTestParentObj*.")

  (gw:wrap-function
   m
   'gw-test-parent-display-obj
   '<gw:void>
   "gw_test_parent_display_obj"
   '(((<gw:TestParentObj*> const) f)))

  
  #t)
