;;;; File: standard.scm
;;;; Copyright (C) 2004-2005 Andreas Rottmann
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
; Scheme48-specific part of the standard wrapset.
;;
;;; Code:

(define-module (g-wrap scheme48 ws standard)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap scm-codegen)
  #:use-module (g-wrap ws standard)
  #:use-module (g-wrap scheme48))


(define-class <gw-s48-ctype-mchars> (<gw-s48-parameterized-type>))
(define-class <gw-s48-ctype-char> (<gw-s48-derived-type>))

;; REMARK: Maybe get rid of wrap-simple-type! in favour of only using
;; wrap-type! and the type-class registry
(define-method (wrap-simple-type! (wrapset <gw-s48-wrapset>) . args)
  (let-keywords args #t (name)
    (let ((type 
           (case name
             ((char unsigned-char)
              (apply make <gw-s48-ctype-char> args))
             ((bool)
              (apply make <gw-s48-primitive-type>
                     #:constructor '_bool args))
             (else
              (apply make <gw-s48-primitive-type> args)))))
      (add-type! wrapset type))))

;;; standard wrapset

(define-class <standard-wrapset> (<gw-s48-wrapset>
                                  <gw-standard-wrapset>)
   #:id 'standard
   #:structure 'g-wrap.gw.standard
   #:open '(ffi ascii srfi-1)
   #:types `((void ,<gw-s48-primitive-type>)
             (mchars ,<gw-s48-ctype-mchars>)))

(define-method (declarations-cg (wrapset <standard-wrapset>))
  '(define (wrap/default-arguments proc n-req defaults)
     (let ((n-defaults (length defaults)))
       (lambda args
         (let ((count (length args)))
           (if (< count n-req)
               (error "too few arguments"))
           (if (> count (+ n-req n-defaults))
               (error "too many arguments"))
           (apply proc (append args (drop defaults (- count n-req)))))))))

(define-method (initialize (wrapset <standard-wrapset>) initargs)
  (next-method)
  (for-each
   (lambda (item)
     (add-structure-export! wrapset item))
   '(((_fun) :syntax)
     ffi-lib ffi-lib? ffi-lib-name
     ffi-obj ffi-obj? ffi-obj-lib ffi-obj-name
     get-ffi-obj
     make-ctype ctype?
     malloc
     cpointer? ptr-ref ptr-set!
     
     _void _bool _int8 _uint8 _int16 _uint16 _int32 _uint32 _int64 _uint64
     _float _double _pointer _fpointer
     _sint8 _sint16 _sint32 _sint64
     _byte _ubyte _sbyte
     _word _uword _sword
     _short _ushort _sshort
     _int _uint _sint
     _long _ulong _slong
     _llong _ullong _sllong
     _cprocedure

     wrap/default-arguments)))

(define-method (wrap-ranged-integer-type! (wrapset <standard-wrapset>) . args)
  (add-type! wrapset (apply make <gw-s48-primitive-type> args)))

;;; mchars

(define-method (initialize (type <gw-s48-ctype-mchars>) initargs)
  (next-method type (append '(#:base _pointer
                              #:parameters (callee-owned? null-ok?)) initargs)))

(define-method (check-typespec-options (type <gw-s48-ctype-mchars>) (options <list>))
  (let ((remainder options))
    (set! remainder (delq 'caller-owned remainder))
    (set! remainder (delq 'callee-owned remainder))
    (set! remainder (delq 'null-ok remainder))
    (next-method type remainder)))

(define-method (typespec->parameters (type <gw-s48-ctype-mchars>) (ts <gw-typespec>))
  (list (if (memq 'callee-owned (options ts)) #t #f)
        (if (memq 'null-ok (options ts)) #t #f)))

(define-method (scheme->c-cg (type <gw-s48-ctype-mchars>))
  `(lambda (v)
     (cond ((and null-ok? (eq? v #f)) #f)
           (callee-owned?             (malloc (+ (string-length v) 1) v))
           ((string? v)               v)
           (else                      (error "invalid 'mchars' string" v)))))

(define-method (c->scheme-cg (type <gw-s48-ctype-mchars>))
  'cpointer->string)

;;; char

(define-method (initialize (type <gw-s48-ctype-char>) initargs)
  (next-method type (append '(#:constructor _char #:base _uint8) initargs)))

(define-method (scheme->c-cg (type <gw-s48-ctype-char>))
  'char->ascii)

(define-method (c->scheme-cg (type <gw-s48-ctype-char>))
  'ascii->char)
