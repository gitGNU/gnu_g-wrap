
(define-module (gw-test-glib-spec)
  #:use-module (g-wrap)
  #:use-module (g-wrap gw-standard-spec)
  #:use-module (g-wrap gw-wct-spec)
  #:use-module (g-wrap gw-glib-spec))

(let ((ws (gw:new-wrapset "gw-test-glib")))

  (gw:wrapset-set-guile-module! ws '(gw-test-glib))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-wct")
  (gw:wrapset-depends-on ws "gw-glib")

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if client-wrapset
         '()
         (list "#include <glib.h>\n"
               "#include \"g-wrap-test-c-code.h\"\n"))))

  (gw:wrap-as-wct ws
                  '<gw:TestIntCarrier*>
                  "gwTestIntCarrier*"
                  "const gwTestIntCarrier*")

  (gw:wrap-function
   ws
   'gw-test-gw-glib-echo-gint64
   '<gw:gint64>
   "gw_test_gw_glib_echo_gint64"
   '((<gw:gint64> arg))
   "Return arg directly.")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; <gw:mchars>

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-gchars-caller-owned
   '(<gw:gchars> caller-owned)
   "gw_test_gw_standard_echo_gchars_caller_owned"
   '(((<gw:gchars> caller-owned) arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-const-gchars-caller-owned
   '(<gw:gchars> const caller-owned)
   "gw_test_gw_standard_echo_const_gchars_caller_owned"
   '(((<gw:gchars> const caller-owned) arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-gchars-callee-owned
   '(<gw:gchars> callee-owned)
   "gw_test_gw_standard_echo_gchars_callee_owned"
   '(((<gw:gchars> callee-owned) arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-const-gchars-callee-owned
   '(<gw:gchars> const callee-owned)
   "gw_test_gw_standard_echo_const_gchars_callee_owned"
   '(((<gw:gchars> const callee-owned) arg))
   "Return arg.")


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; TestIntCarrier -- used by gw:glist-of and gw:gslist-of

  (gw:wrap-function
   ws
   'gw-test-make-int-carrier
   '<gw:TestIntCarrier*> "gw_test_make_int_carrier" '((<gw:int> arg))
   "")

  (gw:wrap-function
   ws
   'gw-test-destroy-int-carrier
   '<gw:void> "gw_test_destroy_int_carrier" '((<gw:TestIntCarrier*> arg))
   "")

  (gw:wrap-function
   ws
   'gw-test-int-carrier-get-value
   '<gw:int>
   "gw_test_int_carrier_get_value"
   '(((<gw:TestIntCarrier*> const) arg))
   "")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; gw:glist-of

  (gw:wrap-function
   ws
   'gw-test-gw-glib-echo-gchars-list-1
   '(gw:glist-of (<gw:gchars> caller-owned) caller-owned)
   "gw_test_gw_glib_echo_list"
   '(((gw:glist-of (<gw:gchars> callee-owned) callee-owned) arg))
   "Return arg directly.")

  (gw:wrap-function
   ws
   'gw-test-gw-glib-echo-gchars-list-2
   '(gw:glist-of (<gw:gchars> callee-owned) callee-owned)
   "gw_test_gw_glib_echo_list"
   '(((gw:glist-of (<gw:gchars> caller-owned) caller-owned) arg))
   "Return arg directly.")

  (gw:wrap-function
   ws
   'gw-test-gw-glib-echo-test-foo-list
   '(gw:glist-of <gw:TestIntCarrier*> callee-owned)
   "gw_test_gw_glib_echo_list"
   '(((gw:glist-of <gw:TestIntCarrier*> caller-owned) arg))
   "Return arg directly.")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; gw:gslist-of

  (gw:wrap-function
   ws
   'gw-test-gw-glib-echo-gchars-slist-1
   '(gw:gslist-of (<gw:gchars> caller-owned) caller-owned)
   "gw_test_gw_glib_echo_slist"
   '(((gw:gslist-of (<gw:gchars> callee-owned) callee-owned) arg))
   "Return arg directly.")

  (gw:wrap-function
   ws
   'gw-test-gw-glib-echo-gchars-slist-2
   '(gw:gslist-of (<gw:gchars> callee-owned) callee-owned)
   "gw_test_gw_glib_echo_slist"
   '(((gw:gslist-of (<gw:gchars> caller-owned) caller-owned) arg))
   "Return arg directly.")

  (gw:wrap-function
   ws
   'gw-test-gw-glib-echo-test-foo-slist
   '(gw:gslist-of <gw:TestIntCarrier*> callee-owned)
   "gw_test_gw_glib_echo_slist"
   '(((gw:gslist-of <gw:TestIntCarrier*> caller-owned) arg))
   "Return arg directly.")

  #t)
