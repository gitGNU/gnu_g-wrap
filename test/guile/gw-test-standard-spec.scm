(define-module (gw-test-standard-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile gw-standard-spec)

  #:export (wrapset-gw-test-standard))


(define ws (make <gw-guile-wrapset>
             #:name "gw-test-standard"
             #:module '(gw-test-standard)))

(define wrapset-gw-test-standard ws)

(depends-on! ws wrapset-gw-standard)

(add-cs-global-declarator! ws (lambda (wrapset)
                                (list "#include \"g-wrap-test-c-code.h\"\n")))

(wrap-constant! ws
                #:name 'gw-test-gw-standard-foo-value
                #:typespec (typespec ws 'int)
                #:value "GW_TEST_GW_STANDARD_FOO_VALUE"
                #:description "The foo value.")

(wrap-constant! ws
                #:name 'gw-test-gw-standard-bar-value
                #:typespec (typespec ws 'mchars 'const 'callee-owned)
                #:value "GW_TEST_GW_STANDARD_BAR_VALUE"
                #:description "The bar value.")

(wrap-function! ws
                #:name 'gw-test-gw-standard-no-op
                #:returns (typespec ws 'void)
                #:c-name "gw_test_gw_standard_no_op"
                #:arguments '()
                #:description "Do nothing.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-scm
;    '<gw:scm>
;    "gw_test_gw_standard_echo_scm"
;    '((<gw:scm> arg))
;    "Return arg.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-bool
;    '<gw:bool>
;    "gw_test_gw_standard_echo_bool"
;    '((<gw:bool> arg))
;    "Return arg.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-char
;    '<gw:char>
;    "gw_test_gw_standard_echo_char"
;    '((<gw:char> arg))
;    "Return arg.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-get-int-min
;    '<gw:scm> "gw_test_gw_standard_get_int_min" '() "Get INT_MIN.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-get-int-max
;    '<gw:scm> "gw_test_gw_standard_get_int_max" '() "Get INT_MAX.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-get-uint-max
;    '<gw:scm> "gw_test_gw_standard_get_uint_max" '() "Get UINT_MAX.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-get-long-min
;    '<gw:scm> "gw_test_gw_standard_get_long_min" '() "Get LONG_MIN.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-get-long-max
;    '<gw:scm> "gw_test_gw_standard_get_long_max" '() "Get LONG_MAX.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-get-ulong-max
;    '<gw:scm> "gw_test_gw_standard_get_ulong_max" '() "Get ULONG_MAX.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-float
;    '<gw:float>
;    "gw_test_gw_standard_echo_float"
;    '((<gw:float> arg))
;    "Return arg.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-double
;    '<gw:double>
;    "gw_test_gw_standard_echo_double"
;    '((<gw:double> arg))
;    "Return arg.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-int
;    '<gw:int>
;    "gw_test_gw_standard_echo_int"
;    '((<gw:int> arg))
;    "Return arg.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-unsigned-int
;    '<gw:unsigned-int>
;    "gw_test_gw_standard_echo_unsigned_int"
;    '((<gw:unsigned-int> arg))
;    "Return arg.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-long
;    '<gw:long>
;    "gw_test_gw_standard_echo_long"
;    '((<gw:long> arg))
;    "Return arg.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-unsigned-long
;    '<gw:unsigned-long>
;    "gw_test_gw_standard_echo_unsigned_long"
;    '((<gw:unsigned-long> arg))
;    "Return arg.")

;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   ;;; <gw:mchars>

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-mchars-caller-owned
;    '(<gw:mchars> caller-owned)
;    "gw_test_gw_standard_echo_mchars_caller_owned"
;    '(((<gw:mchars> caller-owned) arg))
;    "Return arg.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-const-mchars-caller-owned
;    '(<gw:mchars> const caller-owned)
;    "gw_test_gw_standard_echo_const_mchars_caller_owned"
;    '(((<gw:mchars> const caller-owned) arg))
;    "Return arg.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-mchars-callee-owned
;    '(<gw:mchars> callee-owned)
;    "gw_test_gw_standard_echo_mchars_callee_owned"
;    '(((<gw:mchars> callee-owned) arg))
;    "Return arg.")

;   (gw:wrap-function
;    ws
;    'gw-test-gw-standard-echo-const-mchars-callee-owned
;    '(<gw:mchars> const callee-owned)
;    "gw_test_gw_standard_echo_const_mchars_callee_owned"
;    '(((<gw:mchars> const callee-owned) arg))
;    "Return arg.")

