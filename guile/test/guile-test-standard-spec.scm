;;;; File: guile-test-standard-spec.scm
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

(define-module (guile test guile-test-standard-spec)
  #:use-module (oop goops)

  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap util)
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)
  #:use-module (test test-standard-spec))

(define-class <guile-test-standard-wrapset> (<test-standard-wrapset>
					     <gw-guile-wrapset>)
  #:id 'test-standard)

(define-method (global-declarations-cg (ws <guile-test-standard-wrapset>))
  (list
   (next-method)
   "#include \"guile-test-c-code.h\"\n"))

(define-method (initialize (ws <guile-test-standard-wrapset>) initargs)
  (next-method ws (append '(#:module (gw-test-standard)) initargs))

  (add-type! ws (make <error-code-type>
		  #:name 'error-code
		  #:needs-result-var? #f))

  (wrap-function! ws
		  #:name 'gw-test-gw-standard-echo-scm
		  #:returns 'scm
		  #:c-name "gw_test_gw_standard_echo_scm"
		  #:arguments '((scm arg))
		  #:description "Return arg.")

  (wrap-function! ws
		  #:name 'gw-test-retval-exception
		  #:returns 'error-code
		  #:c-name "gw_test_retval_exception"
		  #:arguments '((int arg))
		  #:description "Throw exception if @var{arg} < 0."))


(define-class <error-code-type> (<gw-type>))

(define-method (c-type-name (type <error-code-type>))
  "int")

(define-method (call-cg (type <error-code-type>) (result <gw-value>)
			func-call-code error-var)
  (let ((result-var (gen-c-tmp "result")))
    (list
     "{"
     "  " (c-type-name type) " " result-var " = " func-call-code ";"
     "  if (" func-call-code " != 0)"
     "    scm_throw (scm_from_locale_symbol (\"error-code\"), "
     "               scm_from_int (" result-var "));"
     "}")))

(define-method (post-call-result-cg (type <error-code-type>)
				    (result <gw-value>)
				    status-var)
  '())
