;; Copyright (C) 1997,1998 Christopher Lee
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 2.1,
;; or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this software; see the file COPYING.  If not,
;; write to the Free Software Foundation, 675 Mass Ave, Cambridge, MA
;; 02139, USA.


(define-module (g-wrap g-translate)
  :use-module (g-wrap))

(define *argv* (program-arguments))

(define *noisy?* #f)

(define (note . msg)
  (for-each
   display
   (append (list "; " (car *argv*) ": ") msg (list "\n")))
  (force-output (current-output-port)))

(define (chk msg x)
  (note msg x)
  x)

(define-public (g-translate-verbose) (set! *noisy?* #t))

(define-public type-translations
  '((char* const-string)))

(define-public (export-lst->new-function lst)
  (cond ((eq? (car lst) '%%%error)
	 (display (cadr lst) (current-error-port))
	 (newline (current-error-port))
	 (exit 1)))
  (let ((name  (car lst))
	(flags (cadr lst))
	(rettype (translate-rettype (caddr lst) type-translations))
	(cname (cadddr lst))
	(params (list-ref lst 4))
	(description (list-ref lst 5)))
    (set! params (translate-types params type-translations))
    (let ((newname    (fn-option flags 'name (lambda () #f)))
	  (newtypes   (fn-option flags 'types (lambda () #f)))
	  (newrettype (fn-option flags 'ret-type (lambda () #f))))
      (if newname    (set! name newname))
      (if newtypes   (set! params (patch-params params newtypes)))
      (if newrettype (set! rettype newrettype)))
    (if *noisy?* (note "Exporting '" name "'"))
    (new-function name rettype cname params description)))

(define-public (gwrap-scan-source-file filename)
  (let ((inport (open-input-pipe (string-append "g-scan " filename))))
    (let loop ((lst (read inport)))
      (cond ((eof-object? lst)
	     (close-input-port inport))
	    ((list? lst)
	     (export-lst->new-function lst)
	     (loop (read inport)))
	    (else
	     (loop (read inport))))))) ;; Don't know what it was!

(define (fn-option options name else-thunk)
  (let ((option (assq name options)))
    (if option 
	(cadr option)
	(else-thunk))))

(define (patch-params params newtypes)
  (cond ((null? newtypes) params)
	((and (pair? newtypes)
	      (pair? (car newtypes))
	      (number? (caar newtypes))
	      (< (caar newtypes) (length params)))
	 (set-car! (list-ref params (caar newtypes)) (cadar newtypes))
	 (patch-params params (cdr newtypes)))
	(else
	 (error "patch-params: bad paramter modification syntax" newtypes))))

(define (translate-rettype rettype translations)
  (let ((tr (assq rettype translations)))
    (if tr
	(cadr tr)
	rettype)))

(define (translate-types params translations)
  (if (null? params) 
      '()
      (let* ((p1 (car params))
	     (tr (assq (car p1) translations)))
	(cons
	 (if tr
	     (list (cadr tr) (cadr p1))
	     (car params))
	 (translate-types (cdr params) translations)))))
