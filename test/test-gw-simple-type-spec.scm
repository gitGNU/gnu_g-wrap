
(define-module (test-gw-simple-type-spec)
  :use-module (g-wrap))

;;(use-modules (g-wrapped gw-runtime-spec))

(let ((m (gw:new-module "test-gw-simple-type")))

  (gw:module-set-guile-module! m '(test-gw-simple-type))

  (gw:module-depends-on m "gw-runtime")

  (gw:wrap-simple-type '<gw:simple-int>
                       "int"
                       "const int"
                       '("scm_integer_p(" scm-var ")")
                       '(c-var " = scm_num2long(" scm-var ");\n")
                       '(scm-var " = scm_long2num(" c-var ");\n"))

  (gw:wrap-function
   m
   'test-gw-simple-type-echo
   '<gw:simple-int>
   "test_gw_simple_type_echo"
   '((<gw:simple-int> x))
   "Make a vbox via g-wrap.")

  #t)
