(define-module (gw-test-enumeration)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap enumeration)
  #:use-module (g-wrap guile standard-spec)

  #:export (test-enumeration-wrapset))

(define ws (make <gw-guile-wrapset>
             #:name "test-enumeration"
             #:module '(gw-test-enumeration)))

(define test-enumeration-wrapset ws)

(depends-on! ws standard-wrapset)

(add-cs-global-declarator!
   ws
   (lambda (wrapset)
     (list "#include \"g-wrap-test-c-code.h\"\n")))


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
                #:description "Echo arg.")
