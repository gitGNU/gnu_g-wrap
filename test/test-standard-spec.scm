;;;; File: test-standard-spec.scm
;;;; Copyright (C) 2004-2005 Andreas Rottmann
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

(define-module (test test-standard-spec)
  #:use-module (oop goops)

  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)

  #:export (<test-standard-wrapset>))

(define-class <test-standard-wrapset> (<gw-wrapset>)
  #:dependencies '(standard))

(define-method (global-declarations-cg (ws <test-standard-wrapset>))
  (list
   (next-method)
   "#include \"test/g-wrap-test-c-code.h\"\n"))

(define-method (initialize (ws <test-standard-wrapset>) initargs)
  (next-method)
  
  (wrap-constant! ws
                  #:name 'gw-test-gw-standard-foo-value
                  #:type 'int
                  #:value "GW_TEST_GW_STANDARD_FOO_VALUE"
                  #:description "The foo value.")

  (wrap-constant! ws
                  #:name 'gw-test-gw-standard-bar-value
                  #:type '(mchars const callee-owned)
                  #:value "GW_TEST_GW_STANDARD_BAR_VALUE"
                  #:description "The bar value.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-no-op
                  #:returns 'void
                  #:c-name "gw_test_gw_standard_no_op"
                  #:arguments '()
                  #:description "Do nothing.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-bool
                  #:returns 'bool
                  #:c-name "gw_test_gw_standard_echo_bool"
                  #:arguments '((bool arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-char
                  #:returns 'char
                  #:c-name "gw_test_gw_standard_echo_char"
                  #:arguments '((char arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-int-min
                  #:returns 'int
                  #:c-name "gw_test_gw_standard_get_int_min"
                  #:arguments '()
                  #:description "Get INT_MIN.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-int-max
                  #:returns 'int
                  #:c-name "gw_test_gw_standard_get_int_max"
                  #:arguments '()
                  #:description "Get INT_MAX.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-uint-max
                  #:returns 'unsigned-int
                  #:c-name "gw_test_gw_standard_get_uint_max"
                  #:arguments '()
                  #:description "Get UINT_MAX.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-long-min
                  #:returns 'long
                  #:c-name "gw_test_gw_standard_get_long_min"
                  #:arguments '()
                  #:description "Get LONG_MIN.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-ssize-min
                  #:returns 'ssize_t
                  #:c-name "gw_test_gw_standard_get_ssize_min"
                  #:arguments '()
                  #:description "Get the minimum of @code{ssize_t}.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-long-max
                  #:returns 'long
                  #:c-name "gw_test_gw_standard_get_long_max"
                  #:arguments '()
                  #:description "Get LONG_MAX.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-ulong-max
                  #:returns 'unsigned-long
                  #:c-name "gw_test_gw_standard_get_ulong_max"
                  #:arguments '()
                  #:description "Get ULONG_MAX.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-get-ssize-max
                  #:returns 'ssize_t
                  #:c-name "gw_test_gw_standard_get_ssize_max"
                  #:arguments '()
                  #:description "Get SSIZE_MAX.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-float
                  #:returns 'float
                  #:c-name "gw_test_gw_standard_echo_float"
                  #:arguments '((float arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-double
                  #:returns 'double
                  #:c-name "gw_test_gw_standard_echo_double"
                  #:arguments '((double arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-int
                  #:returns 'int
                  #:c-name "gw_test_gw_standard_echo_int"
                  #:arguments '((int arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-unsigned-int
                  #:returns 'unsigned-int
                  #:c-name "gw_test_gw_standard_echo_unsigned_int"
                  #:arguments '((unsigned-int arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-long
                  #:returns 'long
                  #:c-name "gw_test_gw_standard_echo_long"
                  #:arguments '((long arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-unsigned-long
                  #:returns 'unsigned-long
                  #:c-name "gw_test_gw_standard_echo_unsigned_long"
                  #:arguments '((unsigned-long arg))
                  #:description "Return arg.")

  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-ssize
                  #:returns 'ssize_t
                  #:c-name "gw_test_gw_standard_echo_ssize"
                  #:arguments '((ssize_t arg))
                  #:description "Return arg.")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; mchars

  (wrap-function!
   ws
   #:name 'gw-test-gw-standard-echo-mchars-caller-owned
   #:returns '(mchars caller-owned)
   #:c-name "gw_test_gw_standard_echo_mchars_caller_owned"
   #:arguments '(((mchars caller-owned null-ok) arg))
   #:description "Return arg.")

  (wrap-function!
   ws
   #:name 'gw-test-gw-standard-echo-const-mchars-caller-owned
   #:returns '(mchars const caller-owned)
   #:c-name "gw_test_gw_standard_echo_const_mchars_caller_owned"
   #:arguments '(((mchars const caller-owned null-ok) arg))
   #:description "Return arg.")

  (wrap-function!
   ws
   #:name 'gw-test-gw-standard-echo-mchars-callee-owned
   #:returns '(mchars callee-owned)
   #:c-name "gw_test_gw_standard_echo_mchars_callee_owned"
   #:arguments '(((mchars callee-owned null-ok) arg))
   #:description "Return arg.")

  (wrap-function!
   ws
   #:name 'gw-test-gw-standard-echo-const-mchars-callee-owned
   #:returns '(mchars const callee-owned)
   #:c-name "gw_test_gw_standard_echo_const_mchars_callee_owned"
   #:arguments '(((mchars const callee-owned null-ok) arg))
   #:description "Return arg.")

  ;; Default arguments
  (wrap-function!
   ws
   #:name 'gw-test-strtol
   #:returns 'long
   #:c-name "gw_test_strtol"
   #:arguments '(((mchars const caller-owned) str) (int base (default "0"))))

  ;; out arguments
  (wrap-function!
   ws
   #:name 'gw-test-out-args
   #:returns 'long
   #:c-name "gw_test_out_args"
   #:arguments '((int arg1) ((int out) arg2) ((mchars out callee-owned) arg3)))

  ;; out+default arguments
  (wrap-function!
   ws
   #:name 'gw-test-out+default-args
   #:returns 'long
   #:c-name "gw_test_out_plus_default_args"
   #:arguments '((int arg1) (int arg2 (default "5")) ((mchars out callee-owned) arg3)))

  (wrap-function! ws
                  #:name 'gw-test-many-args
                  #:returns 'int
                  #:c-name "gw_test_many_args"
                  #:arguments (map
                               (lambda (n)
                                 `(int ,(string->symbol
                                         (string-append "arg" (number->string n)))))
                               (iota 11)))
  
  ;; generics
  (wrap-function! ws
                  #:generic-name 'gw-test-generic
                  #:name 'gw-test-generic/int
                  #:returns 'int
                  #:c-name "gw_test_generic__int"
                  #:arguments '((int n)))
  (wrap-function! ws
                  #:generic-name 'gw-test-generic
                  #:name 'gw-test-generic/str-int
                  #:returns '(mchars caller-owned)
                  #:c-name "gw_test_generic__str_int"
                  #:arguments '(((mchars const caller-owned) str)
                                (int n)))
  (wrap-function! ws
                  #:generic-name 'gw-test-generic
                  #:name 'gw-test-generic/str-null-ok
                  #:returns '(mchars callee-owned null-ok)
                  #:c-name "gw_test_generic__str_null_ok"
                  #:arguments '(((mchars const caller-owned null-ok) str)))
  (wrap-function! ws
                  #:generic-name 'gw-test-generic
                  #:name 'gw-test-generic/double-double*
                  #:returns 'void
                  #:c-name "gw_test_generic__double_double_ptr"
                  #:arguments '((double d) ((double out) d-out)))
  
  (wrap-function! ws
                  #:generic-name 'gw-test-generic
                  #:name 'gw-test-generic/bool-bool
                  #:returns 'bool
                  #:c-name "gw_test_generic__bool_bool"
                  #:arguments '((bool b1) (bool b2 (default "0"))))

  )
