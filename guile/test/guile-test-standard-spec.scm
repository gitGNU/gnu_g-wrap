(define-module (guile test guile-test-standard-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)
  #:use-module (test test-standard-spec))

(define-class <guile-test-standard-wrapset> (<test-standard-wrapset>
                                             <gw-guile-wrapset>)
  #:id 'test-standard)

(define-method (initialize (ws <guile-test-standard-wrapset>) initargs)
  (next-method ws (append '(#:module (gw-test-standard)) initargs))
  
  (add-cs-global-declarator! ws
                             (lambda (wrapset)
                               (list "#include \"guile-test-c-code.h\"\n")))

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-scm
                  #:returns 'scm
                  #:c-name "gw_test_gw_standard_echo_scm"
                  #:arguments '((scm arg))
                  #:description "Return arg."))


