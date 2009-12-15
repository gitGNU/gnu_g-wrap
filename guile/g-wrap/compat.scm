;;;; File: compat.scm
;;;; Copyright (C) 2004, 2007, 2009 Andreas Rottmann
;;;;
;;;; based upon G-Wrap 1.3.4,
;;;;   Copyright (C) 1996, 1997,1998 Christopher Lee
;;;;   Copyright (C) 1999, 2000, 2001, 2002 Rob Browning
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
; This module provides a compatibility layer for G-Wrap
; 1.3.4. Currently, only a subset of the original API is provided.
;;
;;; Code:

(define-module (g-wrap compat)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-39)

  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap util)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap enumeration)
  #:use-module (g-wrap guile)

  #:export (gw:register-wrapset
            gw:new-wrapset
            gw:wrapset-depends-on
            gw:wrapset-set-guile-module!
            gw:wrapset-add-cs-declarations!
            gw:wrapset-add-cs-initializers!
            gw:wrap-function
            gw:wrap-as-wct
            gw:wrap-enumeration
            gw:enum-add-value!
            gw:wrap-value
            gw:inline-scheme
            gw:generate-wrapset))

(define-class <gw-compat-wrapset-info> ()
  (name #:getter name #:init-keyword #:name)
  (dependencies #:getter dependencies #:init-value '())
  (guile-module #:getter guile-module #:init-value #f)
  (functions #:init-value '())
  (wcts #:init-value '())
  (simple-types #:init-value '())
  (enumerations #:init-value '())
  (values #:init-value '())
  (cs-declarations #:init-value '())
  (cs-initializers #:init-value '()))

;; name -> <gw-compat-wrapset-info> mapping
(define *compat-wrapsets* (make-hash-table 7))

;; name -> <gw-wrapset>-id mapping
(define *real-wrapsets* (make-hash-table 7))

(define (gw:register-wrapset name id)
  (hash-set! *real-wrapsets* name id))

(define (gw:new-wrapset name)
  (let ((ws (make <gw-compat-wrapset-info> #:name name)))
    (hash-set! *compat-wrapsets* name ws)))

(define (gw:wrapset-depends-on ws name)
  (slot-set! ws 'dependencies (cons name (slot-ref ws 'dependencies))))

(define (gw:wrapset-set-guile-module! ws module)
  (slot-set! ws 'guile-module module))

(define (gw:wrapset-add-cs-declarations! ws codegen)
  (slot-push! ws 'cs-declarations codegen))

(define (gw:wrapset-add-cs-initializers! ws codegen)
  (slot-push! ws 'cs-initializers codegen))

(define (gw:wrap-function ws scm-name return-type c-name arguments
                          . description-opt)
  (let ((desc (cond ((null? description-opt) #f)
                    ((= (length description-opt) 1) (car description-opt))
                    (else (apply string-append description-opt)))))
    (slot-push! ws 'functions (vector scm-name return-type c-name
                                      arguments desc))))

(define (gw:wrap-as-wct ws name c-type-name c-const-type-name)
  (slot-push! ws 'wcts (vector name c-type-name c-const-type-name)))

(define-class <gw-compat-enum-info> ()
  (name #:init-keyword #:name)
  (c-type-name #:init-keyword #:c-type-name)
  (values #:init-value '()))

(define (gw:wrap-enumeration ws name c-type-name)
  (let ((enum  (make <gw-compat-enum-info>
                 #:name name
                 #:c-type-name c-type-name)))
    (slot-push! ws 'enumerations enum)
    enum))

(define (gw:enum-add-value! enum c-name symbol)
  (slot-push! enum 'values (cons symbol c-name)))

(define (gw:wrap-value ws scm-name type c-name)
  (slot-push! ws 'values (vector scm-name type c-name)))

;; This and gw:inline-scheme are copied from guile.scm (quick hack)
(define (scm-form-str->safe-c-str name)
  (define (char->string-replacement char)
    (case char
      ((#\") "\\\"")
      ((#\newline) "\\n")
      (else (string char))))
  (apply
   string-append
   (map
    char->string-replacement
    (string->list name))))

(define-method (gw:inline-scheme . code-chunks)
  (map
   (lambda (chunk)
     (list "scm_c_eval_string(\""
           (scm-form-str->safe-c-str
            (call-with-output-string
             (lambda (port)
               (write chunk port))))
           "\");\n"))
   code-chunks))

(define (convert-typespec spec)
  (cond
   ((symbol? spec)
    (or (assq-ref '((<gw:void> . void)
                    (<gw:int> . int)
                    (<gw:bool> . bool)
                    (<gw:char> . char)
                    (<gw:unsigned-int> . unsigned-int)
                    (<gw:gint64> . int64)
                    (<gw:float> . float)
                    (<gw:double> . double)
                    (<gw:scm> . scm)
                    (<gw:mchars> . (mchars null-ok))
                    (<gw:gchars> . (mchars null-ok))
                    (gw:glist-of . glist-of)
                    (gw:gslist-of . gslist-of)) spec)
        spec))
   ((pair? spec)
    (if (memq (first spec) '(gw:glist-of gw:gslist-of))
        ;; (gw:glist-of foo ...) -> (glist-of (foo) ...)
        ;; (gw:glist-of (foo ...) ...) -> (glist-of (foo ...) ...)
        (apply list
               (convert-typespec (first spec))
               (cond ((symbol? (second spec))
                      (list (convert-typespec (second spec))))
                     (else (convert-typespec (second spec))))
              (cddr spec))
        ;; here we might get a list typespec, e.g. for mchars
        (let ((new-typespec (convert-typespec (first spec))))
          (if (symbol? new-typespec)
              (cons new-typespec (cdr spec))
              (append new-typespec (cdr spec))))))
   (else
    (raise-bad-typespec spec "invalid typespec form"))))

(define-method (convert-arguments args)
  (map (lambda (entry)
         (cons (convert-typespec (car entry)) (cdr entry)))
       args))

(define-class <compat-item> (<gw-item>)
  (for-client? #:init-value #f #:init-keyword #:for-client?)
  (cs-declarations #:init-keyword #:cs-declarations #:init-value #f)
  (cs-initializers #:init-keyword #:cs-initializers #:init-value #f))

(define-macro (slot-cg-invoke obj slot . args)
  (let ((cg (gensym)))
    `(let ((,cg (slot-ref ,obj ,slot)))
       (if ,cg
           (,cg ,@args)
           '()))))

(define-method (global-declarations-cg (ws <gw-wrapset>) (item <compat-item>))
  (slot-cg-invoke item 'cs-declarations ws (slot-ref item 'for-client?)))

(define-method (initializations-cg (ws <gw-wrapset>) (item <compat-item>)
                                   error-var)
  (slot-cg-invoke item 'cs-initializers ws #f error-var))

(define (manifest-as-real-wrapset name)
  (if (not (hash-ref *real-wrapsets* name))
      (let ((ws-info (hash-ref *compat-wrapsets* name)))
        (if (not ws-info)
            (error "unknown wrapset ~A" name))
        (let ((ws-class (make-class
                         (list <gw-guile-wrapset>)
                         '()
                         #:name (string->symbol
                                 (string-append "<" name "-wrapset>"))
                         #:id (string->symbol name)
                         #:dependencies
                         (map (lambda (dep)
                                (manifest-as-real-wrapset dep)
                                (hash-ref *real-wrapsets* dep))
                              (dependencies ws-info))
                         )))
          (add-method!
           initialize
           (method ((ws ws-class) initargs)
             (next-method
              ws
              (append (if (guile-module ws-info)
                          `(#:module ,(guile-module ws-info))
                          '())
                      `(#:shlib-path ,(string-append "lib" name)
                                     #:function-rti? #f)
                      initargs))
              
             (for-each
              (lambda (cs-decl)
                (add-item! ws (make <compat-item> #:cs-declarations cs-decl))
                (add-client-item! ws (make <compat-item>
                                       #:cs-declarations cs-decl
                                       #:for-client #t)))
              (reverse (slot-ref ws-info 'cs-declarations)))
              
             (for-each
              (lambda (cs-init)
                (add-item! ws (make <compat-item> #:cs-initializers cs-init)))
              (reverse (slot-ref ws-info 'cs-initializers)))
              
             (for-each
              (lambda (wct)
                (wrap-as-wct! ws
                              #:name (vector-ref wct 0)
                              #:c-type-name (vector-ref wct 1)
                              #:c-const-type-name (vector-ref wct 2)))
              (reverse (slot-ref ws-info 'wcts)))

             (for-each
              (lambda (simple-type)
                (wrap-simple-type! ws
                                   #:name (vector-ref simple-type 0)
                                   #:c-type-name (vector-ref simple-type 1)
                                   #:type-check (vector-ref simple-type 2)
                                   #:unwrap (vector-ref simple-type 3)
                                   #:wrap (vector-ref simple-type 4)))
              (reverse (slot-ref ws-info 'simple-types)))

             (for-each
              (lambda (enum)
                (wrap-enum! ws
                            #:prefix "gw:enum-"
                            #:name (slot-ref enum 'name)
                            #:c-type-name (slot-ref enum 'c-type-name)
                            #:values (slot-ref enum 'values)))
              (reverse (slot-ref ws-info 'enumerations)))
              
             (for-each
              (lambda (func-info)
                (wrap-function!
                 ws
                 #:name (vector-ref func-info 0)
                 #:returns (convert-typespec (vector-ref func-info 1))
                 #:c-name (vector-ref func-info 2)
                 #:arguments (convert-arguments (vector-ref func-info 3))
                 #:description (vector-ref func-info 4)))
              (reverse (slot-ref ws-info 'functions)))

             (for-each
              (lambda (val)
                (wrap-constant! ws
                                #:name (vector-ref val 0)
                                #:type (convert-typespec (vector-ref val 1))
                                #:value (vector-ref val 2)))
              (reverse (slot-ref ws-info 'values))))))
        (hash-set! *real-wrapsets* name (string->symbol name)))))

(define (gw:generate-wrapset name)
  (manifest-as-real-wrapset name)
  (parameterize ((gw-wcts-nullable? #t))
    (let ((id (hash-ref *real-wrapsets* name)))
      (generate-wrapset 'guile id name))))

