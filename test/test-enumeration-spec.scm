;;;; File: test-enumeration-spec.scm
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

(define-module (test test-enumeration-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap enumeration)

  #:export (<test-enumeration-wrapset>))

(define-class <test-enumeration-wrapset> (<gw-wrapset>)
  #:dependencies '(standard))

(define-method (global-declarations-cg (ws <test-enumeration-wrapset>))
  (list
   (next-method)
   "#include \"test/g-wrap-test-c-code.h\"\n"))

(define-method (initialize (ws <test-enumeration-wrapset>) initargs)
  (next-method ws (append '(#:module (gw-test-enumeration)) initargs))

  (wrap-enum! ws
              #:name '<gw-test-enum>
              #:c-type-name "enum GWTestEnum"
              #:values '((gw-test-enum-zero . "GW_TEST_ENUM_ZERO")
                         (gw-test-enum-one . "GW_TEST_ENUM_ONE")
                         (gw-test-enum-two . "GW_TEST_ENUM_TWO")
                         (gw-test-enum-two-too . "GW_TEST_ENUM_TWO_TOO")
                         (gw-test-enum-two-many . "GW_TEST_ENUM_TWO_MANY")))
  
  (wrap-function! ws
                  #:name 'gw-test-gw-enumeration-echo
                  #:returns '<gw-test-enum>
                  #:c-name "gw_test_gw_enumeration_echo"
                  #:arguments '((<gw-test-enum> arg))
                  #:description "Echo arg."))
