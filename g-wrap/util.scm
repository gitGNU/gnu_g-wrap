(define-module (g-wrap util)
  #:export
  (flatten-display flatten-string separate-by any-str->c-sym-str
   gen-c-tmp str-translate
   expand-special-forms))

(define (flatten-display lst port)
  (cond ((null? lst) '())
	((pair? lst) (flatten-display (car lst) port)
		     (flatten-display (cdr lst) port))
	((or (string? lst)
	     (number? lst)
	     (symbol? lst))
	 (display lst port))
;;	((procedure? lst)
;;	 (flatten-display ((lst 'output)) port))
	(else
	 (error "flatten-display: bad element found in the tree " lst))))

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


;;(gw:error? status-var ...)
;;(gw:error? status-var alloc bad-arg)
;;(gw:error status-var alloc)

;; arg-type arg-range memory misc

;; (gw:wrap-value m 'gtk:green '<gw:int> "GTK_GREEN")

;; (gw:error status-var type ...)

(define (expand-gw-error args param allowed-errors top-form)
  ;; args will be something like (status-var err-sym)

  (if (or (null? args) (null? (cdr args)))
      (error "not enough args to gw:error"))
  (if (not (memq (cadr args) allowed-errors))
      (scm-error 'misc-error "gw:expand-gw-error"
                 "gw:error type \"~A\" not allowed in ~S"
                 (list (cadr args) top-form) 
                 #f))

  (let ((error-var (car args)))
    (set! args (cdr args))
    (list
     "{\n"
     
     (case (car args)
       ((misc)
        ;; (list 'gw:error 'misc msg format-args)
        (if (not (= 3 (length args))) (error "bad call to (gw:error 'misc ...)"))
        (list
         "   (" error-var ").status = GW_ERR_MISC;\n"
         "   (" error-var ").message = " (list-ref args 1) ";\n"
         "   (" error-var ").data = " (list-ref args 2) ";\n"))
       ((memory)
        ;; (list 'gw:error 'memory) 
        (if (not (= 1 (length args)))
            (error "bad call to (gw:error 'memory ...)"))
        (list
         "   (" error-var ").status = GW_ERR_ARG_MEMORY;\n"))
       ((range)
        ;; (list 'gw:error 'range scm-item-out-of-range)
        (if (not (= 2 (length args)))
            (error "bad call to (gw:error 'range ...)"))
        (list
         "   (" error-var ").status = GW_ERR_ARG_TYPE;\n"
         "   (" error-var ").data = " (cadr args) ";\n"))
       ((type)
        ;; (list 'gw:error 'type scm-bad-type-item)
        (if (not (= 2 (length args)))
            (error "bad call to (gw:error 'type ...)"))
        (list
         "   (" error-var ").status = GW_ERR_ARG_TYPE;\n"
         "   (" error-var ").data = " (cadr args) ";\n"))
       ((argc)
        ;; (list 'gw:error 'argc)
        (if (not (= 1 (length args))) (error "bad call to (gw:error 'argc ...)"))
        (list
         "   (" error-var ").status = GW_ERR_ARGC;\n"))
       ((arg-type)
        (if (not (= 1 (length args)))
            (error "bad call to (gw:error 'arg-type ...)"))
        (list
         "   (" error-var ").status = GW_ERR_ARG_TYPE;\n"
         "   (" error-var ").data = " (gw:param-get-scm-name param) ";\n"))
       ((arg-range)
        (if (not (= 1 (length args)))
            (error "bad call to (gw:error 'arg-range ...)"))
        (list
         "   (" error-var ").status = GW_ERR_ARG_RANGE;\n"
         "   (" error-var ").data = " (gw:param-get-scm-name param) ";\n"))
       (else
        (error "unexpected error type in gw:error")))
     
     (if param
         (list "   goto gw__post_call_arg_" (gw:param-get-number param) ";\n")
         "")
         
     "}\n")))


(define (expand-special-forms tree param allowed-errors)
  (define (expand-helper tree param allowed-errors top-form)
    (cond
     ((null? tree) tree)
     ((list? tree)
      (case (car tree)
        ((gw:error?)
         (cond
          ((= 2 (length tree))
           (let ((error-var (list-ref tree 1)))
             (list "((" error-var ").status != GW_ERR_NONE)")))
          ((= 3 (length tree))
           (let ((error-var (list-ref tree 1))
                 (err-sym
                  (case (list-ref tree 2)
                    ((misc) "GW_ERR_MISC")
                    ((memory) "GW_ERR_MEMORY")
                    ((range) "GW_ERR_RANGE")
                    ((type) "GW_ERR_TYPE")
                    ((argc) "GW_ERR_ARGC")
                    ((arg-range) "GW__ARG_RANGE")
                    ((arg-type) "GW__ARG_TYPE")
                    (else (error "improper error type given to gw:error?: "
                                 (list-ref tree 2))))))
             (list "((" error-var ").status == " err-sym ")")))
          (else
           (error "improper use of gw:error?"))))
        ((gw:error)
         (expand-gw-error (cdr tree) param allowed-errors top-form))
        (else
         (map
          (lambda (elt) (expand-helper elt param allowed-errors top-form))
          tree))))
     (else tree)))
  (expand-helper tree param allowed-errors tree))

