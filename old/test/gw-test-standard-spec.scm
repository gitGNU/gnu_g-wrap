
(define-module (gw-test-standard-spec)
  :use-module (g-wrap))

(use-modules (g-wrap gw-standard-spec))

(let ((ws (gw:new-wrapset "gw-test-standard")))

  (gw:wrapset-set-guile-module! ws '(gw-test-standard))

  (gw:wrapset-depends-on ws "gw-standard")

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if client-wrapset
         '()
         "#include \"g-wrap-test-c-code.h\"\n")))

  (gw:wrap-value
   ws
   'gw-test-gw-standard-foo-value
   '<gw:int>
   "GW_TEST_GW_STANDARD_FOO_VALUE"
   "The foo value.")

  (gw:wrap-value
   ws
   'gw-test-gw-standard-bar-value
   '(<gw:mchars> const callee-owned)
   "GW_TEST_GW_STANDARD_BAR_VALUE"
   "The bar value.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-no-op
   '<gw:void>
   "gw_test_gw_standard_no_op"
   '()
   "Do nothing.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-scm
   '<gw:scm>
   "gw_test_gw_standard_echo_scm"
   '((<gw:scm> arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-bool
   '<gw:bool>
   "gw_test_gw_standard_echo_bool"
   '((<gw:bool> arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-char
   '<gw:char>
   "gw_test_gw_standard_echo_char"
   '((<gw:char> arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-get-int-min
   '<gw:scm> "gw_test_gw_standard_get_int_min" '() "Get INT_MIN.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-get-int-max
   '<gw:scm> "gw_test_gw_standard_get_int_max" '() "Get INT_MAX.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-get-uint-max
   '<gw:scm> "gw_test_gw_standard_get_uint_max" '() "Get UINT_MAX.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-get-long-min
   '<gw:scm> "gw_test_gw_standard_get_long_min" '() "Get LONG_MIN.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-get-long-max
   '<gw:scm> "gw_test_gw_standard_get_long_max" '() "Get LONG_MAX.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-get-ulong-max
   '<gw:scm> "gw_test_gw_standard_get_ulong_max" '() "Get ULONG_MAX.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-float
   '<gw:float>
   "gw_test_gw_standard_echo_float"
   '((<gw:float> arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-double
   '<gw:double>
   "gw_test_gw_standard_echo_double"
   '((<gw:double> arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-int
   '<gw:int>
   "gw_test_gw_standard_echo_int"
   '((<gw:int> arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-unsigned-int
   '<gw:unsigned-int>
   "gw_test_gw_standard_echo_unsigned_int"
   '((<gw:unsigned-int> arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-long
   '<gw:long>
   "gw_test_gw_standard_echo_long"
   '((<gw:long> arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-unsigned-long
   '<gw:unsigned-long>
   "gw_test_gw_standard_echo_unsigned_long"
   '((<gw:unsigned-long> arg))
   "Return arg.")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; <gw:mchars>

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-mchars-caller-owned
   '(<gw:mchars> caller-owned)
   "gw_test_gw_standard_echo_mchars_caller_owned"
   '(((<gw:mchars> caller-owned) arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-const-mchars-caller-owned
   '(<gw:mchars> const caller-owned)
   "gw_test_gw_standard_echo_const_mchars_caller_owned"
   '(((<gw:mchars> const caller-owned) arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-mchars-callee-owned
   '(<gw:mchars> callee-owned)
   "gw_test_gw_standard_echo_mchars_callee_owned"
   '(((<gw:mchars> callee-owned) arg))
   "Return arg.")

  (gw:wrap-function
   ws
   'gw-test-gw-standard-echo-const-mchars-callee-owned
   '(<gw:mchars> const callee-owned)
   "gw_test_gw_standard_echo_const_mchars_callee_owned"
   '(((<gw:mchars> const callee-owned) arg))
   "Return arg.")

  ws)
