;;;; File: test-compat-spec.scm
;;;; Copyright (C) 2004, 2007 Andreas Rottmann
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


(define-module (guile test guile-test-compat-spec)
  #:use-module (g-wrap compat)
  #:use-module (g-wrap gw-standard-spec))

(let ((ws (gw:new-wrapset "gw-test-compat")))

  (gw:wrapset-depends-on ws "gw-standard")

  (gw:wrapset-set-guile-module! ws '(gw-test-compat))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list "#include \"test/g-wrap-test-c-code.h\"\n")))

  (gw:wrap-function
   ws
   'gw-test-gw-standard-no-op
   '<gw:void>
   "gw_test_gw_standard_no_op"
   '()
   "Do nothing")

  (gw:wrap-as-wct ws '<gw:TestParentObj*> "gwTestParentObj*" "const gwTestParentObj*")
  
  (gw:wrap-function
   ws
   'gw-test-parent-same-obj
   '<gw:TestParentObj*>
   "gw_test_parent_same_obj"
   '((<gw:TestParentObj*> data))
   "Return the gwTestParentObj*."))
