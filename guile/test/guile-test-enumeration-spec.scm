(define-module (guile test guile-test-enumeration-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)
  #:use-module (test test-enumeration-spec))

(define-class <guile-test-enumeration-wrapset> (<test-enumeration-wrapset>
                                                <gw-guile-wrapset>))

(define-method (initialize (ws <guile-test-enumeration-wrapset>) initargs)
  (next-method)

  (set! (module ws) '(gw-test-enumeration)))
  

(register-wrapset-class guile 'test-enumeration
                        <guile-test-enumeration-wrapset>)
