(define-module (g-wrap util)
  #:use-module (srfi srfi-1)
  
  #:export
  (flatten-display flatten-string separate-by any-str->c-sym-str
   gen-c-tmp str-translate))

(define (flatten-display lst port)
  (define (flatten lst port)
    (cond ((null? lst) '())
          ((pair? lst)
           (flatten (car lst) port)
           (flatten (cdr lst) port))
          ((or (string? lst)
               (number? lst)
               (symbol? lst))
           (display lst port))
          ;;	((procedure? lst)
          ;;	 (flatten-display ((lst 'output)) port))
          (else
           (throw 'bad-element lst))))
  
  (catch 'bad-element
    (lambda () (flatten lst port))
    (lambda (key elt)
      (error  "flatten-display: bad element found in the tree " lst elt))))

(define (flatten-string lst)
  (cond ((null? lst) "")
	((pair? lst) (string-append
		      (flatten-string (car lst))
		      (flatten-string (cdr lst))))
	((string? lst)
	 lst)
	((number? lst)
	 (number->string lst))
	((symbol? lst)
	 (symbol->string lst))
;;	((procedure? lst)
;;	 (flatten-string ((lst 'output))))
	(else
	 (error "flatten-string: bad element found in the tree " lst))))

(define (separate-by lst separator)
  (cond ((null? lst) '())
	((null? (cdr lst)) lst)
	(else
	 (cons (car lst)
	       (cons separator (separate-by (cdr lst) separator))))))

(define (any-str->c-sym-str name)
  (define (char->string-replacement char)
    (cond
     ((char=? char #\?) "_p")
     ((char-alphabetic? char) (string char))
     ((char-numeric? char) (string char))
     (else "_")))

  (apply
   string-append
   (map
    char->string-replacement
    (string->list name))))

(define gen-c-tmp
  (let ((tmp-counter 0))
    (lambda (name)
      (let ((result
             (string-append "gw__tmp" (number->string tmp-counter) "_" name)))
        (set! tmp-counter (+ tmp-counter 1))
        result))))

(define (char-idx str char)
  (let ((strlen (string-length str)))
    (let loop ((i 0))
      (cond ((= i strlen) 
	     #f)
	    ((eq? char (string-ref str i))
	     i)
	    (else (loop (+ 1 i)))))))

(define (str-translate str charstr transvec)
  (let ((buff-size 10000)
	(str-len (string-length str)))
    (let ((buffer (make-string buff-size))
	  (buff-len 0))
      (let* ((buff-add-char 
	      (lambda (char)
		(cond ((= buff-size buff-len)
		       (set! buffer (string-append buffer 
						   (make-string buff-size)))
		       (set! buff-size (* 2 buff-size))))
		(string-set! buffer buff-len char)
		(set! buff-len (+ 1 buff-len))))
	     (buff-add-string
	      (lambda (string)
		(let ((stringlen (string-length string)))
		  (do ((i 0 (+ i 1))) ((= i stringlen))
		    (buff-add-char (string-ref string i)))))))
	(let loop ((i 0))
	  (cond ((= i str-len)
		 (substring buffer 0 buff-len))
		((char-idx charstr (string-ref str i))
		 => (lambda (index)
		      (buff-add-string (vector-ref transvec index))
		      (loop (+ 1 i))))
		(else
		 (buff-add-char (string-ref str i))
		 (loop (+ 1 i)))))))))

