(define-module (g-wrap runtime))

(define gw:runtime-wrapsets-hash (make-hash-table 131))

(define gw:runtime-wrapsets-list '())

(define-public (gw:wrapset-register-runtime name)
  (if (hash-ref gw:runtime-wrapsets-hash name)
      (error "Tried to double-register wrapset " name)
      (begin
        (hash-set! gw:runtime-wrapsets-hash name #t)
        (set! gw:runtime-wrapsets-list
              (cons name gw:runtime-wrapsets-list)))))

(define-public (gw:list-runtime-wrapsets)
   (map string-copy gw:runtime-wrapsets-list))

(define gw:*descriptions* '())

(define-public (gw:add-description binding description)
  (set! gw:*descriptions*
        (cons (cons binding description) gw:*descriptions*)))
