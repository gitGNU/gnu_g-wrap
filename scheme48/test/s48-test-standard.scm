(define-class <s48-test-standard> (<test-case>))

(define (main args)
  (let* ((textui (make <textui-test-runner>))
         (result (run-all-defined-test-cases (list textui))))
    (print-summary textui result)))
