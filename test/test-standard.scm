(define-class <test-standard> (<test-case>))

(define-method* (test-default-arguments (self <test-standard>))
  (assert-equal 123 (gw-test-strtol "123"))
  (assert-equal 7 (gw-test-strtol "111" 2)))

(define-method* (test-output-arguments (self <test-standard>))
  (receive (a b c) (gw-test-out-args 123)
    (assert-equal 123 a)
    (assert-equal 15129 b)
    (assert-equal "foobar" c))
  (receive (a b) (gw-test-out+default-args 7)
    (assert-equal 35 a)
    (assert-equal "foo" b))
  (receive (a b) (gw-test-out+default-args 7 9)
    (assert-equal 63 a)
    (assert-equal "foo" b)))
    
(define-method* (test-bool-type (self <test-standard>))
  (assert-equal #f (gw-test-gw-standard-echo-bool #f))
  (assert-equal #t (gw-test-gw-standard-echo-bool #t))
  (assert-equal #t (gw-test-gw-standard-echo-bool 5))
  (assert-equal #t (gw-test-gw-standard-echo-bool (list 1 2 3))))

(define-method* (test-char-type (self <test-standard>))
  (assert-equal #\space (gw-test-gw-standard-echo-char #\space))
  (assert-equal #\a (gw-test-gw-standard-echo-char #\a))
  (assert-equal #\z (gw-test-gw-standard-echo-char #\z)))

;; TODO: check that overflows signal range errors appropriately...
(define (check-integer-type type-sym echo-func min max)
  ;;(for-each display `("checking that " ,type-sym " works as advertized..."))
  (assert-true (and (= min (echo-func min))
                    (zero? (echo-func 0))
                    (= max (echo-func max)))))

(define-method* (test-integer-types (self <test-standard>))
  (let ((int-min (gw-test-gw-standard-get-int-min))
        (long-min (gw-test-gw-standard-get-long-min))
        (ssize-min (gw-test-gw-standard-get-ssize-min)))

    (for-each check-integer-type
              (list '<gw:int> '<gw:unsigned-int> '<gw:long>
                    '<gw:unsigned-long> '<gw:ssize_t>)
              (list gw-test-gw-standard-echo-int
                    gw-test-gw-standard-echo-unsigned-int
                    gw-test-gw-standard-echo-long
                    gw-test-gw-standard-echo-unsigned-long
                    gw-test-gw-standard-echo-ssize)
              (list int-min
                    0
                    long-min
                    0
                    ssize-min)
              (list (gw-test-gw-standard-get-int-max)
                    (gw-test-gw-standard-get-uint-max)
                    (gw-test-gw-standard-get-long-max)
                    (gw-test-gw-standard-get-ulong-max)
                    (gw-test-gw-standard-get-ssize-max)))))
  
;; TODO add more demanding checks for <gw:mchars> allocation issues.

(define-method* (test-mchars-caller-owned-type (self <test-standard>))
  (let* ((test-str "xyzzy")
         (result-str (gw-test-gw-standard-echo-mchars-caller-owned test-str)))
  (assert-true (and (string? result-str)
                    (string=? test-str result-str)
                    (not (eq? test-str result-str))
                    (not (gw-test-gw-standard-echo-mchars-caller-owned #f))))))


(define-method* (test-mchars-const-caller-owned-type (self <test-standard>))
  (let* ((test-str "xyzzy")
         (result-str
          (gw-test-gw-standard-echo-const-mchars-caller-owned test-str)))
    
  (assert-true (and (string? result-str)
                    (string=? test-str result-str)
                    (not (eq? test-str result-str))
                    (not (gw-test-gw-standard-echo-const-mchars-caller-owned #f))))))

(define-method* (test-mchars-callee-owned-type (self <test-standard>))
  (let* ((test-str "xyzzy")
         (result-str (gw-test-gw-standard-echo-mchars-callee-owned test-str)))
    (assert-true
     (and (string? result-str)
          (string=? test-str result-str)
          (not (eq? test-str result-str))
          (not (gw-test-gw-standard-echo-mchars-callee-owned #f))))))

(define-method* (test-mchars-const-callee-owned-type (self <test-standard>))
  (let* ((test-str "xyzzy")
         (result-str
          (gw-test-gw-standard-echo-const-mchars-callee-owned test-str)))
  (assert-true
   (and (string? result-str)
        (string=? test-str result-str)
        (not (eq? test-str result-str))
        (not (gw-test-gw-standard-echo-const-mchars-callee-owned #f))))))
