(define-module (test test-standard-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)

  #:export (<test-standard-wrapset>))

(define-class <test-standard-wrapset> (<gw-wrapset>))

(define-method (initialize (ws <test-standard-wrapset>) initargs)
  (next-method)
  
  (depends-on! ws 'standard)
  
  (add-cs-global-declarator!
   ws
   (lambda (wrapset)
     (list "#include \"test/g-wrap-test-c-code.h\"\n")))

  (wrap-constant! ws
                  #:name 'gw-test-gw-standard-foo-value
                  #:type 'int
                  #:value "GW_TEST_GW_STANDARD_FOO_VALUE"
                  #:description "The foo value.")

  (wrap-constant! ws
                  #:name 'gw-test-gw-standard-bar-value
                  #:type '(mchars const callee-owned)
                  #:value "GW_TEST_GW_STANDARD_BAR_VALUE"
                  #:description "The bar value.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-no-op
                  #:returns 'void
                  #:c-name "gw_test_gw_standard_no_op"
                  #:arguments '()
                  #:description "Do nothing.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-bool
                  #:returns 'bool
                  #:c-name "gw_test_gw_standard_echo_bool"
                  #:arguments '((bool arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-char
                  #:returns 'char
                  #:c-name "gw_test_gw_standard_echo_char"
                  #:arguments '((char arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-int-min
                  #:returns 'int
                  #:c-name "gw_test_gw_standard_get_int_min"
                  #:arguments '()
                  #:description "Get INT_MIN.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-int-max
                  #:returns 'int
                  #:c-name "gw_test_gw_standard_get_int_max"
                  #:arguments '()
                  #:description "Get INT_MAX.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-uint-max
                  #:returns 'unsigned-int
                  #:c-name "gw_test_gw_standard_get_uint_max"
                  #:arguments '()
                  #:description "Get UINT_MAX.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-long-min
                  #:returns 'long
                  #:c-name "gw_test_gw_standard_get_long_min"
                  #:arguments '()
                  #:description "Get LONG_MIN.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-long-max
                  #:returns 'long
                  #:c-name "gw_test_gw_standard_get_long_max"
                  #:arguments '()
                  #:description "Get LONG_MAX.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-ulong-max
                  #:returns 'unsigned-long
                  #:c-name "gw_test_gw_standard_get_ulong_max"
                  #:arguments '()
                  #:description "Get ULONG_MAX.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-float
                  #:returns 'float
                  #:c-name "gw_test_gw_standard_echo_float"
                  #:arguments '((float arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-double
                  #:returns 'double
                  #:c-name "gw_test_gw_standard_echo_double"
                  #:arguments '((double arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-int
                  #:returns 'int
                  #:c-name "gw_test_gw_standard_echo_int"
                  #:arguments '((int arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-unsigned-int
                  #:returns 'unsigned-int
                  #:c-name "gw_test_gw_standard_echo_unsigned_int"
                  #:arguments '((unsigned-int arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-long
                  #:returns 'long
                  #:c-name "gw_test_gw_standard_echo_long"
                  #:arguments '((long arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-unsigned-long
                  #:returns 'unsigned-long
                  #:c-name "gw_test_gw_standard_echo_unsigned_long"
                  #:arguments '((unsigned-long arg))
                  #:description "Return arg.")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; mchars

  (wrap-function!
   ws
   #:name 'gw-test-gw-standard-echo-mchars-caller-owned
   #:returns '(mchars caller-owned)
   #:c-name "gw_test_gw_standard_echo_mchars_caller_owned"
   #:arguments '(((mchars caller-owned) arg))
   #:description "Return arg.")

  (wrap-function!
   ws
   #:name 'gw-test-gw-standard-echo-const-mchars-caller-owned
   #:returns '(mchars const caller-owned)
   #:c-name "gw_test_gw_standard_echo_const_mchars_caller_owned"
   #:arguments '(((mchars const caller-owned) arg))
   #:description "Return arg.")

  (wrap-function!
   ws
   #:name 'gw-test-gw-standard-echo-mchars-callee-owned
   #:returns '(mchars callee-owned)
   #:c-name "gw_test_gw_standard_echo_mchars_callee_owned"
   #:arguments '(((mchars callee-owned) arg))
   #:description "Return arg.")

  (wrap-function!
   ws
   #:name 'gw-test-gw-standard-echo-const-mchars-callee-owned
   #:returns '(mchars const callee-owned)
   #:c-name "gw_test_gw_standard_echo_const_mchars_callee_owned"
   #:arguments '(((mchars const callee-owned) arg))
   #:description "Return arg."))

