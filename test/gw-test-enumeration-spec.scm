
(define-module (gw-test-enumeration)
  :use-module (g-wrap))

(use-modules (g-wrap gw-standard-spec))

(let ((ws (gw:new-wrapset "gw-test-enumeration")))

  (gw:wrapset-set-guile-module! ws '(gw-test-enumeration))

  (gw:wrapset-depends-on ws "gw-standard")

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if client-wrapset
         '()
         "#include \"g-wrap-test-c-code.h\"\n")))

  (let ((enum (gw:wrap-enumeration ws '<gw-test-enum> "enum GWTestEnum")))

    (gw:enum-add-value! enum "GW_TEST_ENUM_ZERO" 'gw-test-enum-zero)
    (gw:enum-add-value! enum "GW_TEST_ENUM_ONE" 'gw-test-enum-one)
    (gw:enum-add-value! enum "GW_TEST_ENUM_TWO" 'gw-test-enum-two)
    (gw:enum-add-value! enum "GW_TEST_ENUM_TWO_TOO" 'gw-test-enum-two-too)
    (gw:enum-add-value! enum "GW_TEST_ENUM_TWO_MANY" 'gw-test-enum-two-many))

  (gw:wrap-function
   ws
   'gw-test-gw-enumeration-echo
   '<gw-test-enum>
   "gw_test_gw_enumeration_echo"
   '((<gw-test-enum> arg))
   "Echo arg.")

  ws)
