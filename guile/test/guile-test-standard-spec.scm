;;;; File: guile-test-standard-spec.scm
;;;; Copyright (C) 2004 Andreas Rottmann
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
  
  (wrap-function! ws
                  #:name 'gw-test-gw-standard-echo-scm
                  #:returns 'scm
                  #:c-name "gw_test_gw_standard_echo_scm"
                  #:arguments '((scm arg))
                  #:description "Return arg."))


