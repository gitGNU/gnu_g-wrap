;;;; output-file.scm
;;;;
;;;; 	Copyright (C) 1996,1997,1998 Christopher Lee
;;;; 
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this software; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, 675 Mass Ave, Cambridge,
;;;; MA 02139, USA.
;;;; 

;; This is a utility for writing a structured text file.
;; 
;; Open a new output file with
;;   (define outfile (make-outfile "file-name" content-list))
;;   where content-list is a list of section-symbols and strings.
;;   Strings will go verbatim to the output, the symbols will be
;;   replaced with their associated content at outfile:close time.
;;
;; Add text to a section of the file with
;;   (outfile:add-to-section outfile 'section1 "some text here\n")
;;
;; Write the outputfile with
;;   (outfile:close outfile)
;;
;; Function outfile:add-to-section inserts a string or a tree of
;;  strings at the end of a list corresponding to a given section of
;;  the file.  When the file is closed with outfile:close, the
;;  content-list is traversed and each string is written verbatim and
;;  each section-symbol is replaced with its accumulated content.

(define-module (g-wrap output-file))

(define-public (make-outfile name content-list)
  (let ((section-contents (make-hash-table 13)))
    
    ;; add entries for all the valid sections.  A section's contents
    ;; is just a string tree where the top-level items are in reverse
    ;; order from their final output order.
    (for-each
     (lambda (item) (hashq-set! section-contents item '()))
     content-list)
    
    (vector
     'outfile
     name
     content-list
     section-contents)))

(define-public (outfile? x) (eq? (vector-ref x 0) 'outfile))

(define-public (outfile:check x fnname)
  (if (not (outfile? x)) 
      (error (string-append fnname ": item not an outfile"))))

(define-public (outfile:filename-of x)
  (outfile:check x "outfile:filename-of")
  (vector-ref x 1))

(define (outfile:set-filename-of! x val)
  (outfile:check x "outfile:filename-of")
  (vector-set! x 1 val))

(define (outfile:content-list x)
  (outfile:check x "outfile:sections")
  (vector-ref x 2))

(define (outfile:get-section-content of section)
  (hashq-ref (vector-ref of 3) section))

(define (outfile:set-section-content! of section content)
  (hashq-set! (vector-ref of 3) section content))

(define-public (outfile:add-to-section outfile section data)
  (outfile:check outfile "outfile:add-to-section")
  (let ((sect (outfile:get-section-content outfile section)))
    (if (not sect)
	(error "outfile:add-to-section -- section not found " section)
        (outfile:set-section-content! outfile section (cons data sect)))))

;; Make a file with sections according to text (things in %% become
;; sections).  Items in sym-assqs become initial section content
;; entries.
(define-public (text->outfile name text sym-assqs)
  (let* ((content-list (grab-str-vars text))
         (outfile (make-outfile name content-list)))
    
    ;; now take sym-assqs and cram the values for each section there
    ;; into the outfile.
    (for-each
     (lambda (val)
       (outfile:add-to-section outfile (car val) (cdr val)))
     sym-assqs)
    
    outfile))

(define (outfile:output-to-port outfile port)
  (outfile:check outfile "outfile:output-to-port")

  (for-each
   (lambda (item)
     (cond
      ((string? item) (display item port))
      ((symbol? item)
       (flatten-display (reverse (outfile:get-section-content outfile item))
                        port))
      (else
       (error "outfile:output-to-port bad element in content list " item))))
   (outfile:content-list outfile)))

(define-public (outfile:close outfile)
  (outfile:check outfile "outfile:close")
  (if (not (outfile:filename-of outfile))
      (error "outfile already closed"))
  (let ((port (open-output-file (outfile:filename-of outfile))))
    (outfile:output-to-port outfile port)
    (close-output-port port)
    (outfile:set-filename-of! outfile #f)))

(define-public (flatten-display lst port)
  (cond ((null? lst) '())
	((pair? lst) (flatten-display (car lst) port)
		     (flatten-display (cdr lst) port))
	((or (string? lst)
	     (number? lst)
	     (symbol? lst))
	 (display lst port))
	((procedure? lst)
	 (flatten-display ((lst 'output)) port))
	(else
	 (error "flatten-display: bad element found in the tree " lst))))

(define-public (flatten-string lst)
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
	((procedure? lst)
	 (flatten-string ((lst 'output))))
	(else
	 (error "flatten-string: bad element found in the tree " lst))))

(define-public (separate-by lst separator)
  (cond ((null? lst) '())
	((null? (cdr lst)) lst)
	(else
	 (cons (car lst)
	       (cons separator (separate-by (cdr lst) separator))))))

(define (char-idx str char)
  (let ((strlen (string-length str)))
    (let loop ((i 0))
      (cond ((= i strlen) 
	     #f)
	    ((eq? char (string-ref str i))
	     i)
	    (else (loop (+ 1 i)))))))

(define-public (str-translate str charstr transvec)
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


;; (grab-str-vars "void (%this%*)(%<SCM>%,%<generic-data-pointer>%);")
;;   --> ("void (" this "*)(" <SCM> "," <generic-data-pointer> ");")
;; (grab-str-vars "%this% is a %variable% and %%this%% is not!")
;;   --> ("" this " is a " variable " and %" "this%" " is not!")
(define-public (grab-str-vars str)
  (let ((len (string-length str)))
    (reverse
     (let out-loop ((result '()) ;; outer loop looks for %variables%
		    (start 0)
		    (end 0))
       (cond ((= end len)        ;; end of string
	      (cons (substring str start end) result))
	     ((eq? #\% (string-ref str end)) ;; first % -- start of var?
	      (cond ((= (+ 1 end) len)                    ;; % is last char
		     (out-loop result start (+ 1 end)))
		    ((eq? #\% (string-ref str (+ 1 end))) ;; allow %% -> %
		     (out-loop (cons (substring str start (+ 1 end)) result)
			       (+ 2 end) (+ 2 end)))
		    (else                    ;; % marks start of var
		     (let inn-loop ((var-start (+ end 1)) ;; inner loop finds
				    (var-end (+ end 1)))  ;;   end of %vars%
		       (cond ((= var-end len) ;; string ends before 2nd %
			      (cons (substring str start var-end) result))
			     ((eq? #\% (string-ref str var-end))
			      (out-loop       ;; end of %var%
			       (cons (string->symbol 
				      (substring str var-start var-end))
				     (cons (substring str start end)
					   result))
			       (+ var-end 1) (+ var-end 1)))
			     (else
			      (inn-loop var-start (+ var-end 1))))))))
	     (else
	      (out-loop result start (+ end 1))))))))

(define-public (decode-declaration str symbol-trans-proc)
  (apply string-append
	 (map 
	  (lambda (x)
	    (cond ((symbol? x) (symbol-trans-proc x))
		  ((string? x) x)
		  (else "?????")))
	  (grab-str-vars str))))


(define-public (translate-vars lst xlate-assq)
  (cond
   ((null? lst) 
    '())
   ((pair? lst) 
    (cons (translate-vars (car lst) xlate-assq)
	  (translate-vars (cdr lst) xlate-assq)))
   ((symbol? lst)
    (let ((asq-pair (assq lst xlate-assq)))
      (cond
       ((not asq-pair)
	(error "translate-vars: no translation for" lst))
       (else
	(cadr asq-pair)))))
   (else
    lst)))

(define-public (translate-str str xlate-assq)
  (translate-vars (grab-str-vars str) xlate-assq))
