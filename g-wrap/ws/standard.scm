;;;; File: standard.scm
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

(define-module (g-wrap ws standard)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-types)

  #:export (<gw-standard-wrapset>))

(define-class <gw-standard-wrapset> (<gw-wrapset>)
  (use-limits? #:init-value #f))

(define-method (add-type! (wrapset <gw-standard-wrapset>)
                          (type <gw-ranged-integer-type>))
  (next-method)
  (slot-set! wrapset 'use-limits? #t))

(define-class <limits-item> (<gw-item>))

(define-method (before-includes-cg (wrapset <gw-standard-wrapset>)
                                   (item <limits-item>))
    (if (slot-ref wrapset 'use-limits?)
        (list "#define _GNU_SOURCE\n")
        '()))
  
(define-method (global-declarations-cg (wrapset <gw-standard-wrapset>)
                                       (item <limits-item>))
    (if (slot-ref wrapset 'use-limits?)
        (list "#include <limits.h>\n")
        '()))

(define-method (initialize (wrapset <gw-standard-wrapset>) initargs)

  (next-method)

  (let ((limits (make <limits-item>)))
    (add-item! wrapset limits)
    (add-client-item! wrapset limits))
  
  (wrap-type! wrapset 'void
              #:name 'void
              #:c-type-name "void"
              #:ffspec 'void)
  
  (wrap-simple-type! wrapset
                     #:name 'bool
                     #:c-type-name "int"
                     #:ffspec 'sint)
  
  ;; FIXME: Scheme chars are 0-255, not [-128,127] like c chars *may* be
  (wrap-simple-type! wrapset
                     #:name 'char
                     #:c-type-name "char"
                     #:ffspec 'schar) ;; FIXME: see above

  (wrap-simple-type! wrapset
                     #:name 'unsigned-char
                     #:c-type-name "unsigned char"
                     #:ffspec 'uchar)

  (wrap-simple-type! wrapset
                     #:name 'float
                     #:c-type-name "float"
                     #:ffspec 'float)
  
  (wrap-simple-type! wrapset
                     #:name 'double
                     #:c-type-name "double"
                     #:ffspec 'double)

  (wrap-ranged-integer-type! wrapset
                             #:name 'short
                             #:c-type-name "short"
                             #:min "SHRT_MIN" #:max "SHRT_MAX"
                             #:ffspec 'sshort)

  (wrap-ranged-integer-type! wrapset 
                             #:name 'unsigned-short
                             #:c-type-name "unsigned short"
                             #:max "USHRT_MAX"
                             #:ffspec 'ushort)

  (wrap-ranged-integer-type! wrapset
                             #:name 'int
                             #:c-type-name "int"
                             #:min "INT_MIN" #:max "INT_MAX"
                             #:ffspec 'sint)
  
  (wrap-ranged-integer-type! wrapset
                             #:name 'unsigned-int
                             #:c-type-name "unsigned int"
                             #:max "UINT_MAX"
                             #:ffspec 'uint)

  (wrap-ranged-integer-type! wrapset
                             #:name  'long
                             #:c-type-name "long"
                             #:min "LONG_MIN" #:max "LONG_MAX"
                             #:ffspec 'slong)

  (wrap-ranged-integer-type! wrapset
                             #:name 'unsigned-long
                             #:c-type-name "unsigned long"
                             #:max "ULONG_MAX"
                             #:ffspec 'ulong)

  (wrap-ranged-integer-type! wrapset
                             #:name 'long-long
                             #:c-type-name "long long"
                             #:min "LLONG_MIN" #:max "LLONG_MAX"
                             #:ffspec 'slong_long)

  (wrap-ranged-integer-type! wrapset
                             #:name  'unsigned-long-long
                             #:c-type-name "unsigned long long"
                             #:max "ULLONG_MAX"
                             #:ffspec 'ulong_long)

  ;; Bit-counted types from <inttypes.h>
  (wrap-ranged-integer-type! wrapset
                             #:name  'int8
                             #:c-type-name "int8_t"
                             #:min "INT8_MIN" #:max "INT8_MAX"
                             #:ffspec 'sint8)
  
  (wrap-ranged-integer-type! wrapset
                             #:name  'unsigned-int8
                             #:c-type-name "uint8_t"
                             #:max "UINT8_MAX"
                             #:ffspec 'uint8)
  
  (wrap-ranged-integer-type! wrapset
                             #:name  'int16
                             #:c-type-name "int16_t"
                             #:min "INT16_MIN" #:max "INT16_MAX"
                             #:ffspec 'sint16)
  
  (wrap-ranged-integer-type! wrapset
                             #:name  'unsigned-int16
                             #:c-type-name "uint16_t"
                             #:max "UINT16_MAX"
                             #:ffspec 'uint16)
  
  (wrap-ranged-integer-type! wrapset
                             #:name  'int32
                             #:c-type-name "int32_t"
                             #:min "INT32_MIN" #:max "INT32_MAX"
                             #:ffspec 'sint32)
  
  (wrap-ranged-integer-type! wrapset
                             #:name  'unsigned-int32
                             #:c-type-name "uint32_t"
                             #:max "UINT32_MAX"
                             #:ffspec 'uint32)
  
  (wrap-ranged-integer-type! wrapset
                             #:name  'int64
                             #:c-type-name "int64_t"
                             #:min "INT64_MIN" #:max "INT64_MAX"
                             #:ffspec 'sint64)
  
  (wrap-ranged-integer-type! wrapset
                             #:name  'unsigned-int64
                             #:c-type-name "uint64_t"
                             #:max "UINT64_MAX"
                             #:ffspec 'uint64)
  

  (wrap-type! wrapset 'mchars
              #:name 'mchars
              #:c-type-name "char *"
              #:c-const-type-name "const char *" 
              #:ffspec 'pointer))
