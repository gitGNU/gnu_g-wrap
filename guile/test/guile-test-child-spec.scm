(define-module (guile test guile-test-child-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (guile test guile-test-parent-spec)
  #:use-module (test test-child-spec))

(define-class <guile-test-child-wrapset> (<test-child-wrapset>
                                          <gw-guile-wrapset>))

(define-method (initialize (ws <guile-test-child-wrapset>) initargs)
  (next-method)

  (set! (module ws) '(test gw-test-child)))

(register-wrapset-class guile 'test-child  <guile-test-child-wrapset>)
