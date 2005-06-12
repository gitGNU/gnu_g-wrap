;;;; File: scheme48.scm
;;;; Copyright (C) 2005 Andreas Rottmann
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.
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

;;; Commentary:
;;
                                        ; This module extends the G-Wrap core with support for Scheme48.
;;
;;; Code:

(define-module (g-wrap scheme48)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap enumeration)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap scm-codegen)

  #:export (<gw-s48-wrapset>
            structure structure-exports
            add-structure-export!

            <gw-s48-primitive-type>
            type-expression
            <gw-s48-derived-type>
            scheme->c-cg
            c->scheme-cg
            <gw-s48-parameterized-type>
            typespec->parameters
            
            generate-packages))


;;;
;;; Wrapset
;;;

(define-class <gw-s48-wrapset-class> (<gw-wrapset-class>))


(define-method (initialize (class <gw-s48-wrapset-class>) initargs)
  (next-method)
  
  (let-keywords initargs #t (structure open shlib)
    (if structure
        (class-slot-set! class 'structure structure))
    (if open
        (class-slot-set! class 'open open))
    (if shlib
        (class-slot-set! class 'shlib shlib))))

(define-class <gw-s48-wrapset> (<gw-wrapset>)
  (structure #:init-keyword #:structure #:getter structure #:init-value #f
             #:allocation #:each-subclass)
  (open #:init-keyword #:open #:getter open #:init-value '()
        #:allocation #:each-subclass)
  (shlib #:getter shlib #:init-value #f #:allocation #:each-subclass)
  (structure-exports #:getter structure-exports #:init-value '())

  #:metaclass <gw-s48-wrapset-class>
  #:language 'scheme48)

;;; Additional methods

(define-method (add-structure-export! (ws <gw-s48-wrapset>) item)
  (slot-push! ws 'structure-exports item))

(define (wrapset->structure-definition wrapset)
  `(define-structure ,(structure wrapset) (export ,@(structure-exports wrapset))
     (open scheme srfi-23 ,@(open wrapset)
           ,@(map structure (wrapsets-depended-on wrapset)))
     (files ,(name wrapset))))

;;;
;;; Types
;;;

;;; Primitive types

(define-class <gw-s48-primitive-type> (<gw-type>)
  (constructor #:getter type-constructor #:init-keyword #:constructor))

(define-method (check-typespec-options (type <gw-s48-primitive-type>)
                                       (options <list>))
  (let ((remainder options))
    (set! remainder (delq 'const remainder))
    (set! remainder (delq 'unspecialized remainder))
    (set! remainder (delq 'out remainder))
    
    (if (not (null? remainder))
        (raise-bad-typespec type options "spurious options: ~S" remainder))))

(define (uscore-name sym)
  (string->symbol (string-append "_" (symbol->string sym))))

(define-method (initialize (type <gw-s48-primitive-type>) initargs)
  (next-method)
  (let-keywords initargs #t (ffspec)
    (if (and ffspec (not (slot-bound? type 'constructor)))
        (slot-set! type 'constructor
                   (cond ((assq ffspec '((slong_long . _llong)
                                         (ulong_long . _ullong)))
                          => cdr)
                         (else (uscore-name ffspec)))))))

(define (i/o-type-extend expr ts)
  (if (memq 'out (options ts))
      `(_ptr o ,expr)
      expr))

(define-method (type-expression (type <gw-s48-primitive-type>)
                                (ts <gw-typespec>))
  (i/o-type-extend (type-constructor type) ts))

;; Derived (user-defined) types

(define-class <gw-s48-derived-type> (<gw-s48-primitive-type>)
  (base-type #:getter base-type #:init-keyword #:base))

(define-method (initialize (type <gw-s48-derived-type>) initargs)
  (next-method)
  (let-keywords initargs #t (constructor)
    (if (not constructor)
        (slot-set! type 'constructor (uscore-name (name type))))))

;; (scheme->c-cg type)
(define-generic scheme->c-cg)

;; (c->scheme-cg type)
(define-generic c->scheme-cg)

(define-method (add-type! (ws <gw-s48-wrapset>) (type <gw-s48-derived-type>))
  (next-method)
  (add-structure-export! ws (type-constructor type)))

(define-method (add-function! (ws <gw-s48-wrapset>) (func <gw-function>))
  (next-method)
  (add-structure-export! ws (name func)))

(define-method (declarations-cg (wrapset <gw-s48-wrapset>)
                                (type <gw-s48-derived-type>))
  `(define ,(type-constructor type) (make-ctype ,(base-type type)
                                      ,(scheme->c-cg type)
                                      ,(c->scheme-cg type))))

(define-class <gw-s48-parameterized-type> (<gw-s48-derived-type>)
  (parameters #:getter parameters #:init-keyword #:parameters))

(define-method (type-expression (type <gw-s48-parameterized-type>)
                                (ts <gw-typespec>))
  (i/o-type-extend `(,(type-constructor type) ,@(typespec->parameters type ts)) ts))

(define-method (declarations-cg (wrapset <gw-s48-wrapset>)
                                (type <gw-s48-parameterized-type>))
  `(define (,(type-constructor type) ,@(parameters type))
     (make-ctype ,(base-type type)
       ,(scheme->c-cg type)
       ,(c->scheme-cg type))))

;; (typespec->parameters type typespec) -> list of values
(define-generic typespec->parameters)


(define (arg-symbol n)
  (string->symbol (string-append "arg" (number->string n))))

(define-method (declarations-cg (wrapset <gw-s48-wrapset>)
                                (function <gw-function>))
  (define wrapper-code
    `(get-ffi-obj
      ,(c-name function) ,(shlib wrapset)
      ,(if (> (output-argument-count function) 0)
           `(_fun ,@(map (lambda (ts n)
                           (if (memq 'out (options ts))
                               `(,(arg-symbol n) : ,(type-expression (type ts) ts))
                               (type-expression (type ts) ts)))
                         (argument-typespecs function)
                         (iota (argument-count function)))
                  -> (r : ,(type-expression (return-type function)
                                            (return-typespec function)))
                  -> (values r ,@(filter-map (lambda (ts n)
                                               (if (memq 'out (options ts))
                                                   (arg-symbol n)
                                                   #f))
                                             (argument-typespecs function)
                                             (iota (argument-count function)))))
           `(_fun ,@(map (lambda (ts)
                           (type-expression (type ts) ts))
                         (argument-typespecs function))
                  -> ,(type-expression (return-type function)
                                       (return-typespec function))))))
  `(define ,(name function)
     ,(let* ((opt-count (optional-argument-count function))
             (n-req (- (input-argument-count function) opt-count))
             (arg-count (argument-count function))
             (opt-args (list->vector (optional-arguments function))))
        (if (> opt-count 0)
            (let ((arg-syms (map arg-symbol (iota n-req))))
              `(let ((wrapper ,wrapper-code)
                     (defaults
                       (list
                        ,@(map (lambda (n)
                                 (format #t "default: ~S ~S\n" (name function) n)
                                 (call-with-input-string 
                                  (default-value (vector-ref opt-args n))
                                  read))
                               (iota opt-count)))))
                 (wrap/default-arguments wrapper ,n-req defaults)))
            wrapper-code))))

;;;
;;; Generation
;;;

(define-method (generate-packages (filename <string>) . ws-names)
  (let ((had-error? #f)
        (basedir (dirname filename)))
    (guard
     (c
      (#t (handle-condition c)
          (set! had-error? #t)))
     (let ((wrapsets (map (lambda (name) (get-wrapset 'scheme48 name)) ws-names)))
       (call-with-output-file/cleanup filename
         (lambda (port)
           (define (dsp . args)
             (for-each (lambda (s) (display s port)) args))
           (dsp ";; Generated by G-Wrap, an experimental wrapper engine\n"
                "\n")
           (for-each
            (lambda (ws)
              (let ((filename (string-append basedir "/" (symbol->string (name ws)))))
                (pretty-print (wrapset->structure-definition ws) port)
                (generate-wrapset ws filename)))
            wrapsets)))))
    (if had-error?
        (exit 1))))
