;;;; File: test-child-spec.scm
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

(define-module (test test-child-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-types)

  #:export (<test-child-wrapset>))

(define-class <test-child-wrapset> (<gw-wrapset>)
  #:dependencies '(test-parent))
  
(define-method (global-declarations-cg (ws <test-child-wrapset>))
  (list
   (next-method)
   "#include \"test/g-wrap-test-c-code.h\"\n"))

(define-method (initialize (ws <test-child-wrapset>) initargs)
  (next-method)
  
  (wrap-as-wct! ws
                #:name '<gw:TestChildObj*>
                #:c-type-name "gwTestChildObj*"
                #:c-const-type-name "const gwTestChildObj*")
  
  (wrap-function! ws
                  #:name 'gw-test-child-make-obj
                  #:returns '<gw:TestChildObj*>
                  #:c-name "gw_test_child_make_obj"
                  #:arguments '(((mchars const caller-owned) data))
                  #:description "Make a gwTestChildObj*.")

  (wrap-function! ws
                  #:name 'gw-test-child-same-obj
                  #:returns '<gw:TestChildObj*>
                  #:c-name "gw_test_child_same_obj"
                  #:arguments '((<gw:TestChildObj*> data))
                  #:description "Make a gwTestChildObj*.")

  (wrap-function! ws
                  #:name 'gw-test-child-display-obj
                  #:returns 'void
                  #:c-name "gw_test_child_display_obj"
                  #:arguments '(((<gw:TestChildObj*> const) f)))

  (wrap-function! ws
                  #:name 'gw-test-child-pass-back-parent-obj
                  #:returns '<gw:TestParentObj*>
                  #:c-name "gw_test_child_pass_back_parent_obj"
                  #:arguments '((<gw:TestParentObj*> f))))
