(define-module (test test-enumeration-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap enumeration)

  #:export (<test-enumeration-wrapset>))

(define-class <test-enumeration-wrapset> (<gw-wrapset>))

(define-method (global-declarations-cg (ws <test-enumeration-wrapset>))
  (list
   (next-method)
   "#include \"test/g-wrap-test-c-code.h\"\n"))

(define-method (initialize (ws <test-enumeration-wrapset>) initargs)
  (next-method ws (append '(#:module (gw-test-enumeration)) initargs))

  (depends-on! ws 'standard)

  (wrap-enum! ws
              #:name '<gw-test-enum>
              #:c-type-name "enum GWTestEnum"
              #:values '((gw-test-enum-zero . "GW_TEST_ENUM_ZERO")
                         (gw-test-enum-one . "GW_TEST_ENUM_ONE")
                         (gw-test-enum-two . "GW_TEST_ENUM_TWO")
                         (gw-test-enum-two-too . "GW_TEST_ENUM_TWO_TOO")
                         (gw-test-enum-two-many . "GW_TEST_ENUM_TWO_MANY")))
  
  (wrap-function! ws
                  #:name 'gw-test-gw-enumeration-echo
                  #:returns '<gw-test-enum>
                  #:c-name "gw_test_gw_enumeration_echo"
                  #:arguments '((<gw-test-enum> arg))
                  #:description "Echo arg."))
