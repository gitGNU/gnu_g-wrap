#!/usr/bin/guile -s
!#

(use-modules (ice-9 slib))
(require 'pretty-print)

(define *mod* 'mod)

(define (handle-add-type scm-sym c-type-name c->scm scm->c type-test)
  (pretty-print
   `(let ((wt (gw:wrap-native-type ,*mod* ,scm-sym ,c-type-name)))
      (gw:native-type-set-scm-rep-type-test-ccodegen!
       wt
       (lambda (type param)
         (let ((old-func ,type-test))
           (old-func (gw:param-scm-name param)))))
      
      (gw:native-type-set-scm->c-ccodegen!
       wt
       (lambda (c-name scm-name)
         (let ((old-func ,scm->c))
           (list c-name " = " (old-func scm-name) ";" (string #\newline)))))
      
      (gw:native-type-set-c->scm-ccodegen!
       wt
       (lambda (c-name scm-name)
         (let ((old-func ,c->scm))
           (list scm-name " = " (old-func c-name) ";" (string #\newline))))))))

(define (handle-make-pointer-token-type scm-sym c-type-name)
  (pretty-print
   `(let ((nnt (gw:wrap-non-native-type ,*mod* ,scm-sym ,c-type-name)))
      #t)))

(define (handle-gwrap-include-global-header name)
  (pretty-print
   `(gw:wrapper-add-c-declarations
     ,*mod*
     (string-append "#include <" ,name ">" (string #\newline)))))

(define (handle-new-function scm-sym result c-name args docstring)
  (pretty-print
   `(gw:wrap-function ,*mod* ,scm-sym ,result ,c-name ,args ,docstring)))

(define (handle-form form)
  (cond

   ((eq? 'add-type (car form))
    (apply handle-add-type (cdr form)))

   ((eq? 'make-pointer-token-type (car form))
    (apply handle-make-pointer-token-type (cdr form)))

   ((eq? 'gwrap-include-global-header (car form))
    (apply handle-gwrap-include-global-header (cdr form)))

   ((eq? 'new-function (car form))
    (apply handle-new-function (cdr form)))

   (else (pretty-print form)))
  (newline))


(do ((next-form (read) (read)))
    ((eof-object? next-form))
  (handle-form next-form))
