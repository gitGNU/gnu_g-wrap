;;;; File: test-aggregating-spec.scm
;;;; Copyright (C) 2005 Ludovic Courtès
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

(define-module (test test-aggregating-spec)
  #:use-module (oop goops)

  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap c-types)

  #:export (<test-aggregating-wrapset>))

(define-class <test-aggregating-wrapset> (<gw-wrapset>)
  #:dependencies '(standard))

(define-method (global-declarations-cg (ws <test-aggregating-wrapset>))
  (list
   (next-method)
   "#include \"test/g-wrap-test-c-code.h\"\n"))

(define-method (initialize (ws <test-aggregating-wrapset>) initargs)
  (next-method)

  (wrap-as-wct! ws
		#:name '<gw:TestAggregatingObj*>
		#:allowed-options '(aggregated out)
		#:c-type-name "gwTestAggregatingObj*"
		#:c-const-type-name "const gwTestAggregatingObj*"
		#:wcp-free-function "gw_test_cleanup_aggregating_obj")

  (wrap-function! ws
		  #:name 'gw-test-make-simple-aggregating-obj
		  #:returns '<gw:TestAggregatingObj*>
		  #:c-name "gw_test_make_simple_aggregating_obj"
		  #:arguments '()
		  #:description "Make a gwTestAggregatingObj* that does not
aggregate any other object.")

  (wrap-function! ws
		  #:name 'gw-test-make-aggregating-obj
		  #:returns '<gw:TestAggregatingObj*>
		  #:c-name "gw_test_make_aggregating_obj"
		  #:arguments '(((<gw:TestAggregatingObj*> aggregated)
				 aggregated))
		  #:description "Make a gwTestAggregatingObj* that aggregates
(i.e. keeps a pointer to) @var{aggregated}.")

  (wrap-function! ws
		  #:name 'gw-test-make-aggregating-obj/alt
		  #:returns 'void
		  #:c-name "gw_test_make_aggregating_obj_alt"
		  #:arguments '(((<gw:TestAggregatingObj*> aggregated)
				 aggregated)
                                ((<gw:TestAggregatingObj*> out) new))
		  #:description "Make a gwTestAggregatingObj* that aggregates
(i.e. keeps a pointer to) @var{aggregated}.")

  (wrap-function! ws
		  #:name 'gw-test-get-aggregated-obj
		  #:returns 'void
		  #:c-name "gw_test_get_aggregated_obj"
		  #:arguments '(((<gw:TestAggregatingObj*> const) obj))
		  #:description "Return the object aggregated by @var{obj}."))
