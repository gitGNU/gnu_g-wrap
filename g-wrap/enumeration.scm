;;;; File: enumeration.scm
;;;; Copyright (C) 2004 Andreas Rottmann
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
; This module provides support for enumeration types, which are
; presented as symbols on the Scheme side.
;;
;;; Code:

(define-module (g-wrap enumeration)
  #:use-module (oop goops)
  
  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap util)

  #:export (<gw-enumeration-type> val-array-name wrap-enum!))

(define-class <gw-enumeration-type> (<gw-simple-rti-type>)
  (values #:init-keyword #:values)
  (val-array-name #:getter val-array-name
                  #:init-form (gen-c-tmp "val_array")))

(define-method (initialize (enum <gw-enumeration-type>) initargs)
  (next-method)

  (slot-set! enum 'ffspec 'sint)) ;; Correct?

(define (val-array-cg enum)
  (list
   "static const GWEnumPair " (slot-ref enum 'val-array-name) "[] = {\n"
   (map
    (lambda (enum-val)
      (let ((c-sym (cdr enum-val))
            (scm-sym (car enum-val)))
        (list
         "  {" c-sym ", \"" scm-sym "\" },\n")))
    (slot-ref enum 'values))
   " { 0, NULL } };\n"))
  
(define-method (global-declarations-cg (wrapset <gw-wrapset>)
                                       (enum <gw-enumeration-type>))
  (val-array-cg enum))

(define-method (client-global-declarations-cg (wrapset <gw-wrapset>)
                                              (enum <gw-enumeration-type>))
  (val-array-cg enum))

(define-method (wrap-enum! (ws <gw-wrapset>) . args)
  (let ((type (apply make <gw-enumeration-type> args)))
    (add-type! ws type)
    type))

