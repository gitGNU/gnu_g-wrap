;;;; File: test-parent-spec.scm
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

(define-module (test test-parent-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-types)

  #:export (<test-parent-wrapset>))

(define-class <test-parent-wrapset> (<gw-wrapset>))
  
(define-method (global-declarations-cg (ws <test-parent-wrapset>))
  (list
   (next-method)
   "#include \"test/g-wrap-test-c-code.h\"\n"))

(define-method (initialize (ws <test-parent-wrapset>) initargs)
  (next-method)
  
  (depends-on! ws 'standard)

  (wrap-as-wct! ws
                #:name '<gw:TestParentObj*>
                #:c-type-name "gwTestParentObj*"
                #:c-const-type-name "const gwTestParentObj*")
  
  (wrap-function! ws
                  #:name 'gw-test-parent-make-obj
                  #:returns '<gw:TestParentObj*>
                  #:c-name "gw_test_parent_make_obj"
                  #:arguments '(((mchars const caller-owned) data))
                  #:description "Make a gwTestParentObj*.")
  
  (wrap-function! ws
                  #:name 'gw-test-parent-same-obj
                  #:returns '<gw:TestParentObj*>
                  #:c-name "gw_test_parent_same_obj"
                  #:arguments '((<gw:TestParentObj*> data))
                  #:description "Make a gwTestParentObj*.")
  
  (wrap-function! ws
                  #:name 'gw-test-parent-display-obj
                  #:returns 'void
                  #:c-name "gw_test_parent_display_obj"
                  #:arguments '(((<gw:TestParentObj*> const) f))))
