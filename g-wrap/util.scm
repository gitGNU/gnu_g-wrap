(define-module (g-wrap util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (oop goops)
  
  #:export
  (&gw-bad-element
   element tree

   guard/handle call-with-output-file/cleanup
   
   <gw-cs-labels>
   goto-cg label-cg
   
   flatten-display flatten-string separate-by any-str->c-sym-str
   gen-c-tmp str-translate))

;;; Condition stuff

(define-class &gw-bad-element (&error)
  (element #:getter element)
  (tree #:getter tree))

(define-macro (guard/handle . body)
  (let ((cond-name (gensym)))
    `(guard
      (,cond-name
       (else (handle-condition ,cond-name)))
      ,@body)))

;;; General utilities

(define (call-with-output-file/cleanup file-name proc)
  
  (define (cleanup)
    (if (file-exists? file-name)
        (delete-file file-name)))
    
  (let ((had-errors? #f))
    (lazy-catch #t
      (lambda () 
        (guard
         (c
          ((condition-has-type? c &error)
           (set! had-errors? #t)
           (handle-condition c)))
         
         (call-with-output-file file-name proc)))
         
      (lambda (key . args)
        ;; here we handle non-conditon errors (e.g. user cg code
        ;; errors) by passing the exception thru to the top level
        (cleanup)
        (apply throw key args)))
    
    (if had-errors?
        (begin
          (cleanup)
          (exit 1)))))

;;; Support for C labels - declare them only when needed

(define-class <gw-cs-labels> ()
  (labels #:init-value '()))

(define-method (goto-cg (self <gw-cs-labels>) label)
  (slot-set! self 'labels (cons label (slot-ref self 'labels)))
  (list "goto gw__" label ";\n"))

(define-method (label-cg (self <gw-cs-labels>) label)
  (if (member label (slot-ref self 'labels))
      (list "gw__" label ":\n")
      '()))

;;; Output routines

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
      (raise (condition (&gw-bad-element (tree lst) (element elt)))))))

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


;;; String utilities

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

