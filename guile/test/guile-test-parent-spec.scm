(define-module (guile test guile-test-parent-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)
  #:use-module (test test-parent-spec))

(define-class <guile-test-parent-wrapset> (<test-parent-wrapset>
                                             <gw-guile-wrapset>)
  #:id 'test-parent)

(define-method (initialize (ws <guile-test-parent-wrapset>) initargs)
  (next-method)

  (set! (module ws) '(test gw-test-parent)))


